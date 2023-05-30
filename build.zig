const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "self",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.linkLibC();
    b.installArtifact(exe);
    exe.addAnonymousModule("zig-args", .{
        .source_file = .{ .path = "./vendor/zig-args/args.zig" },
    });

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    var test_harness_exe = b.addExecutable(.{
        .name = "self-test",
        .root_source_file = .{ .path = "tests/harness.zig" },
        .target = target,
        .optimize = optimize,
    });
    test_harness_exe.linkLibC();
    test_harness_exe.addAnonymousModule("zigself", .{
        .source_file = .{ .path = "src/package.zig" },
    });

    const test_harness_run_cmd = b.addRunArtifact(test_harness_exe);
    if (b.args) |args| {
        test_harness_run_cmd.addArgs(args);
    }

    const test_harness_step = b.step("test", "Run zigSelf test harness");
    test_harness_step.dependOn(&test_harness_run_cmd.step);
}
