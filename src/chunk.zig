const std = @import("std");

pub const OpCode = enum {
    OP_RETURN,
};

pub const Chunk = struct {
    const Self = @This();

    count: usize,
    capacity: usize,
    code: []u8,

    pub const empty: Self = .{
        .count = 0,
        .capacity = 0,
        .code = &.{},
    };

    pub fn write(self: *Chunk, allocator: std.mem.Allocator, byte: u8) !void {
        if (self.capacity < self.count + 1) {
            const new_capacity = if (self.capacity == 0) 8 else self.capacity * 2;
            self.code = try allocator.realloc(self.code, new_capacity);
            self.capacity = new_capacity;
        }

        self.code[self.count] = byte;
        self.count += 1;
    }

    pub fn deinit(self: *Chunk, allocator: std.mem.Allocator) void {
        allocator.free(self.code);
        self.* = .empty;
    }
};
