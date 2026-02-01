const std = @import("std");
const chunk = @import("chunk.zig");

pub fn disassembleChunk(self: chunk.Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < self.code.items.len) {
        offset = disassembleInstruction(self, offset);
    }
}

fn disassembleInstruction(c: chunk.Chunk, offset: usize) usize {
    std.debug.print("{d:04} ", .{offset});

    if (offset > 0 and c.lines.items[offset] == c.lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:4} ", .{c.lines.items[offset]});
    }

    const instruction: chunk.OpCode = @enumFromInt(c.code.items[offset]);
    switch (instruction) {
        .OP_RETURN => return simpleInstruction("OP_RETURN", offset),
        .OP_CONSTANT => return constantInstruction("OP_CONSTANT", c, offset),
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, c: chunk.Chunk, offset: usize) usize {
    const constant = c.code.items[offset + 1];
    std.debug.print("{s: <16} {d: >4} '", .{ name, constant });
    c.constants.items[constant].print();
    std.debug.print("'\n", .{});
    return offset + 2;
}
