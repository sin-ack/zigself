const std = @import("std");

pub fn build(b: *std.Build) void {
    // Options
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Modules
    const zigself = b.addModule("zigself", .{
        .root_source_file = b.path("src/zigself.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
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

    const test_harness_step = b.step("test", "Run zigSelf test harness");
    test_harness_step.dependOn(&test_harness_run_cmd.step);
}
