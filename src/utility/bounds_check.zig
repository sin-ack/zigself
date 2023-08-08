const std = @import("std");

pub fn exceedsBoundsOf(value: anytype, comptime T: type) bool {
    const value_type_info = @typeInfo(@TypeOf(value));
    const target_type_info = @typeInfo(T);

    if (value_type_info.Int.signedness != target_type_info.Int.signedness)
        @compileError("Can't bounds check for types of different signedness");

    if (value_type_info.Int.bits < target_type_info.Int.bits)
        return false;

    // Prevent triggering UB ourselves by using the largest type between the two.
    const PeerType = @TypeOf(value, @as(T, undefined));

    if (@as(PeerType, value) > @as(PeerType, std.math.maxInt(T)))
        return true;

    // The check above is sufficient for unsigned values, but not signed ones.
    if (value_type_info.Int.signedness != .signed)
        return false;

    if (@as(PeerType, value) < @as(PeerType, std.math.minInt(T)))
        return true;

    return false;
}
