const std = @import("std");

pub const Value = union(enum) {
    nil,
    bool: bool,
    double: f64,

    pub fn print(self: Value) void {
        switch (self) {
            .double => |double| std.debug.print("{d}", .{double}),
            .nil => std.debug.print("nil", .{}),
            .bool => |val| std.debug.print("{s}", .{if (val) "true" else "false"}),
        }
    }

    pub fn isFalsish(self: Value) bool {
        return switch (self) {
            .nil => true,
            .bool => |val| !val,
            else => false,
        };
    }
};
