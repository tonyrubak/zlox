const std = @import("std");
const zlox = @import("zlox");

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    return repl(allocator);
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
        try out.print("> ", .{});
        try out.flush();

        _ = in.streamDelimiter(&line.writer, '\n') catch |err| {
            try out.print("\n", .{});
            if (err == error.EndOfStream) break else return err;
        };

        vm.interpret(allocator, line.written()) catch {};

        in.toss(1);
        line.clearRetainingCapacity();
    }
}
