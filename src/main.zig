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
    try chunk.write(allocator, @intFromEnum(zlox.chunk.OpCode.OP_NEGATE), 123);
    try chunk.write(allocator, @intFromEnum(zlox.chunk.OpCode.OP_RETURN), 123);
    try vm.interpret(allocator, &chunk);
}

fn repl(allocator: std.mem.Allocator) !void {
    var out_buffer: [1024]u8 = undefined;
    var in_buffer: [1024]u8 = undefined;

    const stdout = std.fs.File.stdout();
    var writer = stdout.writer(&out_buffer);
    const out = &writer.interface;

    const stdin = std.fs.File.stdin();
    var reader = stdin.reader(&in_buffer);
    const in = &reader.interface;

    var vm = zlox.vm.VM.init;
    defer vm.deinit(allocator);

    var line = std.Io.Writer.Allocating.init(allocator);
    defer line.deinit();

    while (true) {
        out.print("> ", .{});

        in.streamDelimiter(&line.writer, '\n') catch |err| {
            out.print("\n", .{});
            if (err == error.EndOfStream) break else return err;
        };

        in.toss(1);
    }
}
