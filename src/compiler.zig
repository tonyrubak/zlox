const std = @import("std");
const scanner_mod = @import("scanner.zig");

pub fn compile(source: []const u8) void {
    var scanner = scanner_mod.Scanner.init(source);
    while (true) {
        const token = scanner.scanToken();
        if (token.line != scanner.line) {
            std.debug.print("{d:4} ", .{token.line});
            scanner.line = token.line;
        } else {
            std.debug.print("    | ", .{});
        }
        std.debug.print("{d:2} '{s}'\n", .{ @intFromEnum(token.t), token.start });

        if (token.t == .TOKEN_EOF) break;
    }
}
