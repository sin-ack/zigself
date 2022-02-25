// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const zigself = @import("zigself");
const Heap = zigself.Heap;
const environment = zigself.environment;
const Script = zigself.Script;
const interpreter = zigself.interpreter;

const Test = struct {
    basename: []const u8,
    path: []const u8,
    expects_error: bool,

    /// Initializes a new Test object.
    /// Dupes path and basename.
    pub fn init(allocator: Allocator, basename: []const u8, path: []const u8) !Test {
        const basename_copy = try allocator.dupe(u8, basename);
        errdefer allocator.free(basename_copy);

        const path_copy = try allocator.dupe(u8, path);

        const expects_error = std.mem.endsWith(u8, path, ".error.self");

        return Test{
            .basename = basename_copy,
            .path = path_copy,
            .expects_error = expects_error,
        };
    }

    /// Deinitializes the Test object.
    pub fn deinit(self: *Test, allocator: Allocator) void {
        allocator.free(self.basename);
        allocator.free(self.path);
    }
};

/// Expects an iterable directory.
fn collectTests(allocator: Allocator, directory: std.fs.Dir) !std.ArrayList(Test) {
    var walker = try directory.walk(allocator);
    defer walker.deinit();

    var tests = std.ArrayList(Test).init(allocator);
    errdefer {
        for (tests.items) |*the_test| {
            the_test.deinit(allocator);
        }
        tests.deinit();
    }

    next_file: while (try walker.next()) |entry| {
        if (entry.kind != .File)
            continue :next_file;
        if (!std.mem.endsWith(u8, entry.basename, ".self"))
            continue :next_file;

        var the_test = try Test.init(allocator, entry.basename, entry.path);
        errdefer the_test.deinit(allocator);

        try tests.append(the_test);
    }

    return tests;
}

fn runTests(allocator: Allocator, tests: std.ArrayList(Test)) !bool {
    const harness_dirname = std.fs.path.dirname(@src().file) orelse ".";
    const project_root = try std.fs.path.resolve(allocator, &[_][]const u8{ harness_dirname, ".." });
    defer allocator.free(project_root);
    const stdlib_entrypoint = try std.fs.path.resolve(allocator, &[_][]const u8{ project_root, "objects", "everything.self" });
    defer allocator.free(stdlib_entrypoint);

    var progress = std.Progress{};

    const heap = try Heap.create(allocator);
    defer heap.destroy();

    var lobby = try environment.prepareRuntimeEnvironment(heap);
    defer environment.teardownGlobalObjects(heap);

    const stdlib_script = try Script.createFromFilePath(allocator, stdlib_entrypoint);
    defer stdlib_script.unref();

    var did_parse_without_errors = try stdlib_script.value.parseScript();
    try stdlib_script.value.reportDiagnostics(std.io.getStdErr().writer());
    if (!did_parse_without_errors) {
        std.debug.panic("!!! Encountered errors while parsing the standard library entrypoint!", .{});
    }

    if ((try interpreter.executeScript(allocator, heap, stdlib_script, lobby)) == null) {
        std.debug.panic("!!! Standard library script failed to execute!", .{});
    }

    const root_progress_node = progress.start("Run zigSelf tests", tests.items.len);
    root_progress_node.activate();

    // Passes
    var passed_tests = std.ArrayList([]const u8).init(allocator);
    defer passed_tests.deinit();
    // Failures
    var failed_tests = std.ArrayList([]const u8).init(allocator);
    defer failed_tests.deinit();
    var parse_failed_tests = std.ArrayList([]const u8).init(allocator);
    defer parse_failed_tests.deinit();
    var passing_expect_error_tests = std.ArrayList([]const u8).init(allocator);
    defer passing_expect_error_tests.deinit();
    var crashed_tests = std.ArrayList([]const u8).init(allocator);
    defer crashed_tests.deinit();

    next_test: for (tests.items) |the_test| {
        var script_progress_node = root_progress_node.start(the_test.basename, 0);
        script_progress_node.activate();
        defer script_progress_node.end();

        // NOTE: Our tests run too fast, so maybeRefresh doesn't get a chance to
        //       print the test name. This causes us to not be able to easily
        //       tell which test failed. So let's directly use refresh() and
        //       print each test name.
        progress.refresh();

        const path_to_test = try std.fs.path.resolve(allocator, &[_][]const u8{ harness_dirname, the_test.path });
        defer allocator.free(path_to_test);

        const script = try Script.createFromFilePath(allocator, path_to_test);
        defer script.unref();

        did_parse_without_errors = try script.value.parseScript();
        try script.value.reportDiagnostics(std.io.getStdErr().writer());
        if (!did_parse_without_errors) {
            try parse_failed_tests.append(the_test.basename);
            continue :next_test;
        }

        const result = interpreter.executeScript(allocator, heap, script, lobby) catch |err| {
            const test_name_without_extension = the_test.basename[0 .. the_test.basename.len - 5];
            std.debug.print("Caught error when executing test {s}: {}\n", .{ test_name_without_extension, err });
            if (@errorReturnTrace()) |trace| {
                std.debug.dumpStackTrace(trace.*);
            }

            try crashed_tests.append(the_test.basename);
            continue :next_test;
        };

        if (result == null and !the_test.expects_error) {
            try failed_tests.append(the_test.basename);
            continue :next_test;
        } else if (result != null and the_test.expects_error) {
            try passing_expect_error_tests.append(the_test.basename);
            continue :next_test;
        }

        try passed_tests.append(the_test.basename);
    }

    root_progress_node.end();

    std.debug.print("Summary: {} total, {} passed, {} failed, {} failed to parse, {} crashed, {} passing expected-error.\n", .{
        tests.items.len,
        passed_tests.items.len,
        failed_tests.items.len,
        parse_failed_tests.items.len,
        crashed_tests.items.len,
        passing_expect_error_tests.items.len,
    });

    var did_fail = false;

    if (failed_tests.items.len > 0) {
        did_fail = true;
        std.debug.print("Failed tests:\n", .{});
        for (failed_tests.items) |name| std.debug.print("  {s}\n", .{name});
    }

    if (parse_failed_tests.items.len > 0) {
        did_fail = true;
        std.debug.print("Tests that failed to parse:\n", .{});
        for (parse_failed_tests.items) |name| std.debug.print("  {s}\n", .{name});
    }

    if (crashed_tests.items.len > 0) {
        did_fail = true;
        std.debug.print("Crashed tests:\n", .{});
        for (crashed_tests.items) |name| std.debug.print("  {s}\n", .{name});
    }

    if (passing_expect_error_tests.items.len > 0) {
        did_fail = true;
        std.debug.print("Passing expected-error tests:\n", .{});
        for (passing_expect_error_tests.items) |name| std.debug.print("  {s}\n", .{name});
    }

    return did_fail;
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var directory: std.fs.Dir = blk: {
        const cwd = std.fs.cwd();
        const source_file = @src().file;

        break :blk try cwd.openDir(std.fs.path.dirname(source_file) orelse ".", .{ .iterate = true });
    };
    defer directory.close();

    var tests = try collectTests(allocator, directory);
    defer {
        for (tests.items) |*the_test| {
            the_test.deinit(allocator);
        }
        tests.deinit();
    }

    return if (try runTests(allocator, tests)) 1 else 0;
}
