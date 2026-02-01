const std = @import("std");
const zlox = @import("zlox");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var chunk = zlox.chunk.Chunk.empty;
    defer chunk.deinit(allocator);
    try chunk.write(allocator, @intFromEnum(zlox.chunk.OpCode.OP_RETURN));
    const constant = try chunk.addConstant(allocator, .{ .double = 1.2 });
    try chunk.write(allocator, @intFromEnum(zlox.chunk.OpCode.OP_CONSTANT));
    try chunk.write(allocator, constant);
    zlox.debug.disassembleChunk(chunk, "test chunk");
}
