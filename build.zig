const std = @import("std");

const zig_args = std.build.Pkg{
    .name = "zig-args",
    .path = .{ .path = "./vendor/zig-args/args.zig" },
};

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("self", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    exe.addPackage(zig_args);

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    var test_harness_exe = b.addExecutable("self-test", "tests/harness.zig");
    test_harness_exe.setTarget(target);
    test_harness_exe.setBuildMode(mode);
    test_harness_exe.addPackage(.{
        .name = "zigself",
        .path = .{ .path = "src/package.zig" },
    });

    const test_harness_run_cmd = test_harness_exe.run();
    if (b.args) |args| {
        test_harness_run_cmd.addArgs(args);
    }

    const test_harness_step = b.step("test", "Run zigSelf test harness");
    test_harness_step.dependOn(&test_harness_run_cmd.step);
}
