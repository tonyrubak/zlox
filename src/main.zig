const std = @import("std");
const zlox = @import("zlox");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var chunk = zlox.chunk.Chunk.empty;
    try chunk.write(allocator, @intFromEnum(zlox.chunk.OpCode.OP_RETURN));
    zlox.debug.disassembleChunk(chunk, "test chunk");
    chunk.deinit(allocator);
}

test "create and write to chunk" {
    const gpa = std.testing.allocator;

    var chunk = zlox.chunk.Chunk.empty;
    defer chunk.deinit(gpa);
    try chunk.write(gpa, @intFromEnum(zlox.chunk.OpCode.OP_RETURN));
    try std.testing.expectEqual(chunk.code[0], @intFromEnum(zlox.chunk.OpCode.OP_RETURN));
}
