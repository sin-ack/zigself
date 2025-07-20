// Copyright (c) 2021-2024, sin-ack <sin-ack@protonmail.com>
//
// Contains code from the ZLS project.
// Copyright (c) 2024, ZLS contributors
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

pub fn build(b: *std.Build) void {
    // Options
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const enable_tracy = b.option(bool, "tracy", "Whether tracy should be enabled.") orelse false;
    const enable_tracy_allocation = b.option(bool, "tracy-allocation", "Enable using TracyAllocator to monitor allocations.") orelse enable_tracy;
    const enable_tracy_callstack = b.option(bool, "tracy-callstack", "Enable callstack graphs.") orelse enable_tracy;

    // Modules
    const tracy = getTracyModule(b, .{
        .target = target,
        .optimize = optimize,
        .enable = enable_tracy,
        .enable_allocation = enable_tracy_allocation,
        .enable_callstack = enable_tracy_callstack,
    });
    const zigself = b.addModule("zigself", .{
        .root_source_file = b.path("src/zigself.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
        .imports = &.{
            .{ .name = "tracy", .module = tracy },
        },
    });
    const zig_args = b.dependency("zig-args", .{}).module("args");

    // Steps
    const exe = b.addExecutable(.{
        .name = "self",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("zigself", zigself);
    exe.root_module.addImport("zig-args", zig_args);
    exe.root_module.addImport("tracy", tracy);
    // NOTE: Currently forced to use LLVM backend because the native backend
    //       segfaults (and we can't upgrade yet due to our usingnamespace
    //       use).
    exe.use_llvm = true;

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    var test_harness_exe = b.addExecutable(.{
        .name = "self-test",
        .root_source_file = b.path("tests/harness.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_harness_exe.root_module.addImport("zigself", zigself);

    const test_harness_run_cmd = b.addRunArtifact(test_harness_exe);
    if (b.args) |args| {
        test_harness_run_cmd.addArgs(args);
    }
    test_harness_run_cmd.cwd = b.path(".");

    const zig_tests = b.addTest(.{
        .name = "Zig-based tests",
        .root_module = zigself,
    });
    const zig_tests_run_cmd = b.addRunArtifact(zig_tests);

    const test_step = b.step("test", "Run zigSelf tests");
    test_step.dependOn(&test_harness_run_cmd.step);
    test_step.dependOn(&zig_tests_run_cmd.step);
}

fn getTracyModule(
    b: *std.Build,
    options: struct {
        target: std.Build.ResolvedTarget,
        optimize: std.builtin.OptimizeMode,
        enable: bool,
        enable_allocation: bool,
        enable_callstack: bool,
    },
) *std.Build.Module {
    const tracy_options = b.addOptions();
    tracy_options.step.name = "tracy options";
    tracy_options.addOption(bool, "enable", options.enable);
    tracy_options.addOption(bool, "enable_allocation", options.enable and options.enable_allocation);
    tracy_options.addOption(bool, "enable_callstack", options.enable and options.enable_callstack);

    const tracy_module = b.addModule("tracy", .{
        .root_source_file = b.path("src/utility/tracy.zig"),
        .target = options.target,
        .optimize = options.optimize,
    });
    tracy_module.addImport("options", tracy_options.createModule());
    if (!options.enable) return tracy_module;
    const tracy_dependency = b.lazyDependency("tracy", .{}) orelse return tracy_module;

    tracy_module.link_libc = true;
    tracy_module.link_libcpp = true;

    // On mingw, we need to opt into windows 7+ to get some features required by tracy.
    const tracy_c_flags: []const []const u8 = if (options.target.result.isMinGW())
        &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined", "-D_WIN32_WINNT=0x601" }
    else
        &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined" };

    tracy_module.addIncludePath(tracy_dependency.path(""));
    tracy_module.addCSourceFile(.{
        .file = tracy_dependency.path("public/TracyClient.cpp"),
        .flags = tracy_c_flags,
    });

    if (options.target.result.os.tag == .windows) {
        tracy_module.linkSystemLibrary("dbghelp", .{});
        tracy_module.linkSystemLibrary("ws2_32", .{});
    }

    return tracy_module;
}
