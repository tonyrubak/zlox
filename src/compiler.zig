const std = @import("std");
const scanner_mod = @import("scanner.zig");

pub fn compile(scanner: *scanner_mod.Scanner) void {
    while (true) {
        const token = scanner.scanToken();
        if (token.line != scanner.line) {
            std.debug.print("{d:4} ", .{});
            scanner.line = token.line;
        } else {
            std.debug.print("    | ", .{});
        }
        std.debug.print("{d:2} '{s}'\n", .{ token.type, token.start });

        if (token.type == .EOF) break;
    }
}
