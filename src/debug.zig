const std = @import("std");
const chunk_mod = @import("chunk.zig");

pub fn disassembleChunk(self: *chunk_mod.Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < self.*.code.items.len) {
        offset = disassembleInstruction(self, offset);
    }
}

pub fn disassembleInstruction(c: *chunk_mod.Chunk, offset: usize) usize {
    std.debug.print("{d:04} ", .{offset});

    if (offset > 0 and c.*.lines.items[offset] == c.*.lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:4} ", .{c.lines.items[offset]});
    }

    const instruction: chunk_mod.OpCode = @enumFromInt(c.*.code.items[offset]);
    switch (instruction) {
        .OP_NEGATE => return simpleInstruction("OP_NEGATE", offset),
        .OP_RETURN => return simpleInstruction("OP_RETURN", offset),
        .OP_CONSTANT => return constantInstruction("OP_CONSTANT", c, offset),
        .OP_ADD => return simpleInstruction("OP_ADD", offset),
        .OP_SUBTRACT => return simpleInstruction("OP_SUBTRACT", offset),
        .OP_MULTIPLY => return simpleInstruction("OP_MULTIPLY", offset),
        .OP_DIVIDE => return simpleInstruction("OP_DIVIDE", offset),
        .OP_NIL => return simpleInstruction("OP_NIL", offset),
        .OP_FALSE => return simpleInstruction("OP_FALSE", offset),
        .OP_TRUE => return simpleInstruction("OP_TRUE", offset),
        .OP_NOT => return simpleInstruction("OP_NOT", offset),
        .OP_EQUAL => return simpleInstruction("OP_EQUAL", offset),
        .OP_GREATER => return simpleInstruction("OP_GREATER", offset),
        .OP_LESS => return simpleInstruction("OP_LESS", offset),
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, c: *chunk_mod.Chunk, offset: usize) usize {
    const constant = c.*.code.items[offset + 1];
    std.debug.print("{s: <16} {d: >4} '", .{ name, constant });
    c.*.constants.items[constant].print();
    std.debug.print("'\n", .{});
    return offset + 2;
}
