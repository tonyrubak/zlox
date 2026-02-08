const std = @import("std");
const value = @import("value.zig");

pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_NEGATE,
    OP_RETURN,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_PRINT,
    OP_POP,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: std.ArrayList(value.Value),
    lines: std.ArrayList(usize),

    pub const empty = Chunk{
        .code = .empty,
        .constants = .empty,
        .lines = .empty,
    };

    pub fn write(self: *Chunk, allocator: std.mem.Allocator, byte: u8, line: usize) !void {
        try self.code.append(allocator, byte);
        try self.lines.append(allocator, line);
    }

    pub fn addConstant(self: *Chunk, allocator: std.mem.Allocator, v: value.Value) !u8 {
        try self.constants.append(allocator, v);
        return @intCast(self.constants.items.len - 1);
    }

    pub fn deinit(self: *Chunk, allocator: std.mem.Allocator) void {
        self.code.deinit(allocator);
        self.constants.deinit(allocator);
        self.lines.deinit(allocator);
    }
};

test "create and write to chunk" {
    const gpa = std.testing.allocator;

    var chunk = Chunk.empty;
    defer chunk.deinit(gpa);
    try chunk.write(gpa, @intFromEnum(OpCode.OP_RETURN), 1);
    const opcode: OpCode = @enumFromInt(chunk.code.items[0]);
    try std.testing.expectEqual(opcode, .OP_RETURN);
}
