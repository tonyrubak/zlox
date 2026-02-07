const std = @import("std");
const object_mod = @import("object.zig");

pub const Value = union(enum) {
    nil,
    bool: bool,
    double: f64,
    object: *object_mod.Obj,

    pub fn print(self: Value) void {
        switch (self) {
            .double => |double| std.debug.print("{d}", .{double}),
            .nil => std.debug.print("nil", .{}),
            .bool => |val| std.debug.print("{s}", .{if (val) "true" else "false"}),
            else => @panic("Not implemented"),
        }
    }

    pub fn isFalsish(self: Value) bool {
        return switch (self) {
            .nil => true,
            .bool => |val| !val,
            else => false,
        };
    }

    pub fn isObjType(self: Value, object_type: object_mod.ObjType) bool {
        return switch (self) {
            .object => |obj| return obj.obj_type == object_type,
            else => false,
        };
    }

    pub fn isNumber(self: Value) bool {
        return switch (self) {
            .double => true,
            else => false,
        };
    }
};
