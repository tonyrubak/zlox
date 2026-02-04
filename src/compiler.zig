const std = @import("std");
const chunk_mod = @import("chunk.zig");
const scanner_mod = @import("scanner.zig");
const value_mod = @import("value.zig");

pub const Compiler = struct {
    current: scanner_mod.Token,
    previous: scanner_mod.Token,
    scanner: scanner_mod.Scanner,
    had_error: bool,
    panic_mode: bool,
    chunk: *chunk_mod.Chunk,

    pub fn init(source: []const u8, chunk: *chunk_mod.Chunk) Compiler {
        const result = Compiler{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner_mod.Scanner.init(source),
            .had_error = false,
            .panic_mode = false,
            .chunk = chunk,
        };

        return result;
    }

    pub fn compile(self: *Compiler) bool {
        self.advance();
        //self.expression();
        self.consume(.TOKEN_EOF, "Expected end of expression.");
        return !self.had_error;
    }

    fn advance(self: *Compiler) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.t != .TOKEN_ERROR) break;

            self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn consume(self: *Compiler, token_type: scanner_mod.TokenType, message: []const u8) void {
        if (self.current.t == token_type) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn emitByte(self: *Compiler, allocator: std.mem.Allocator, byte: u8) !void {
        try self.chunk.write(allocator, byte, self.previous.line);
    }

    fn emitBytes(self: *Compiler, allocator: std.mem.Allocator, byte1: u8, byte2: u8) !void {
        try self.emitByte(allocator, byte1);
        try self.emitByte(allocator, byte2);
    }

    fn emitReturn(self: *Compiler, allocator: std.mem.Allocator) !void {
        try self.emitByte(allocator, @intFromEnum(chunk_mod.OpCode.OP_RETURN));
    }

    fn makeConstant(self: *Compiler, allocator: std.mem.Allocator, value: value_mod.Value) !u8 {
        const constant = try self.chunk.addConstant(allocator, value);
        if (constant > std.math.maxInt(u8)) {
            self.errorAtPrev("Too many constants in one chunk.");
            return 0;
        }

        return constant;
    }

    fn emitConstant(self: *Compiler, allocator: std.mem.Allocator, value: value_mod.Value) !void {
        const constant = try self.makeConstant(allocator, value);
        try self.emitBytes(allocator, @intFromEnum(chunk_mod.OpCode.OP_CONSTANT), constant);
    }

    fn endCompiler(self: *Compiler, allocator: std.mem.Allocator) !void {
        try self.emitReturn(allocator);
    }

    fn number(self: *Compiler, allocator: std.mem.Allocator) !void {
        const value = try std.fmt.parseFloat(f64, self.previous.lexeme);
        try self.emitConstant(allocator, .{ .double = value });
    }

    fn errorAtCurrent(self: *Compiler, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    fn errorAtPrev(self: *Compiler, message: []const u8) void {
        self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Compiler, token: *const scanner_mod.Token, message: []const u8) void {
        if (self.panic_mode) return;
        self.panic_mode = true;
        std.debug.print("[line {d} Error", .{token.line});

        if (token.t == .TOKEN_EOF) {
            std.debug.print(" at end", .{});
        } else if (token.t == .TOKEN_ERROR) {} else {
            std.debug.print(" at '{s}'", .{token.lexeme});
        }

        std.debug.print(": {s}\n", .{message});
        self.had_error = true;
    }
};

test "advance the thing, get a token" {
    const allocator = std.testing.allocator;

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    var compiler = Compiler.init("!", &chunk);
    compiler.advance();

    try std.testing.expectEqual(.TOKEN_BANG, compiler.current.t);
}

test "advance the thing twice, get a previous" {
    const allocator = std.testing.allocator;

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    var compiler = Compiler.init("! =", &chunk);
    compiler.advance();
    compiler.advance();

    try std.testing.expectEqual(.TOKEN_BANG, compiler.previous.t);
}

test "write a constant to the chunk pls" {
    const allocator = std.testing.allocator;

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    var compiler = Compiler.init("42", &chunk);
    compiler.advance();
    compiler.advance();
    try compiler.number(allocator);
    try std.testing.expectEqual(@intFromEnum(chunk_mod.OpCode.OP_CONSTANT), compiler.chunk.code.items[0]);
    try std.testing.expectEqual(0, compiler.chunk.code.items[1]);
    try std.testing.expectEqual(value_mod.Value{ .double = 42 }, compiler.chunk.constants.items[0]);
}
