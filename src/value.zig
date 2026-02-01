const std = @import("std");

pub const Value = union(enum) {
    double: f64,

    pub fn print(self: Value) void {
        switch (self) {
            .double => |double| std.debug.print("{d}", .{double}),
        }
    }
};

pub const ValueArray = std.ArrayList(Value);
