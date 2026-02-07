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
            .object => |object| {
                const str = object.asObjString();
                std.debug.print("{s}", .{str.chars[0..str.length]});
            },
        }
    }

    pub fn isFalsish(self: Value) bool {
        return switch (self) {
            .nil => true,
            .bool => |val| !val,
            else => false,
        };
    }

    pub fn isEqual(self: Value, other: Value) bool {
        if (!std.mem.eql(u8, @tagName(self), @tagName(other))) {
            return false;
        }

        switch (self) {
            .bool => return self.bool == other.bool,
            .double => return self.double == other.double,
            .nil => return true,
            else => unreachable,
        }
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
