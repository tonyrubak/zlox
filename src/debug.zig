const std = @import("std");
const chunk = @import("chunk.zig");

pub fn disassembleChunk(c: chunk.Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < c.count) {
        offset = disassembleInstruction(c, offset);
    }
}

fn disassembleInstruction(c: chunk.Chunk, offset: usize) usize {
    std.debug.print("{d:04} ", .{offset});

    const instruction: chunk.OpCode = @enumFromInt(c.code[offset]);
    switch (instruction) {
        .OP_RETURN => return simpleInstruction("OP_RETURN", offset),
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
