const std = @import("std");
const zlox = @import("zlox");

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = zlox.vm.VM.init;
    defer vm.deinit(allocator);
    var chunk = zlox.chunk.Chunk.empty;
    defer chunk.deinit(allocator);

    const constant = try chunk.addConstant(allocator, .{ .double = 1.2 });
    try chunk.write(allocator, @intFromEnum(zlox.chunk.OpCode.OP_CONSTANT), 123);
    try chunk.write(allocator, constant, 123);
    try chunk.write(allocator, @intFromEnum(zlox.chunk.OpCode.OP_RETURN), 123);
    zlox.debug.disassembleChunk(chunk, "test chunk");
    try vm.interpret(&chunk);
}
