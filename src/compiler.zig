const std = @import("std");
const chunk_mod = @import("chunk.zig");
const scanner_mod = @import("scanner.zig");
const value_mod = @import("value.zig");
const object_mod = @import("object.zig");

const Precedence = enum(u8) { None, Assignment, Or, And, Equality, Comparison, Term, Factor, Unary, Call, Primary };

fn nextPrecedence(prec: Precedence) Precedence {
    return @enumFromInt(@intFromEnum(prec) + 1);
}

fn precedenceLessOrEqual(a: Precedence, b: Precedence) bool {
    return @intFromEnum(a) <= @intFromEnum(b);
}

const ParseFn = *const fn (self: *Compiler, allocator: std.mem.Allocator) anyerror!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

fn getRule(token_type: scanner_mod.TokenType) ParseRule {
    return switch (token_type) {
        .TOKEN_LEFT_PAREN => .{ .prefix = Compiler.grouping, .infix = null, .precedence = .None },
        .TOKEN_RIGHT_PAREN => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_LEFT_BRACE => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_RIGHT_BRACE => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_COMMA => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_DOT => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_MINUS => .{ .prefix = Compiler.unary, .infix = Compiler.binary, .precedence = .Term },
        .TOKEN_PLUS => .{ .prefix = null, .infix = Compiler.binary, .precedence = .Term },
        .TOKEN_SEMICOLON => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_SLASH => .{ .prefix = null, .infix = Compiler.binary, .precedence = .Factor },
        .TOKEN_STAR => .{ .prefix = null, .infix = Compiler.binary, .precedence = .Factor },
        .TOKEN_BANG => .{ .prefix = Compiler.unary, .infix = null, .precedence = .None },
        .TOKEN_BANG_EQUAL => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_EQUAL => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_EQUAL_EQUAL => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_GREATER => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_GREATER_EQUAL => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_LESS => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_LESS_EQUAL => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_IDENTIFIER => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_STRING => .{ .prefix = Compiler.string, .infix = null, .precedence = .None },
        .TOKEN_NUMBER => .{ .prefix = Compiler.number, .infix = null, .precedence = .None },
        .TOKEN_AND => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_CLASS => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_FALSE => .{ .prefix = Compiler.literal, .infix = null, .precedence = .None },
        .TOKEN_TRUE => .{ .prefix = Compiler.literal, .infix = null, .precedence = .None },
        .TOKEN_NIL => .{ .prefix = Compiler.literal, .infix = null, .precedence = .None },
        else => .{ .prefix = null, .infix = null, .precedence = .None },
    };
}
pub const Compiler = struct {
    current: scanner_mod.Token,
    previous: scanner_mod.Token,
    scanner: scanner_mod.Scanner,
    had_error: bool,
    panic_mode: bool,
    chunk: *chunk_mod.Chunk,
    objects: *std.ArrayList(*object_mod.Obj),

    pub fn init(source: []const u8, chunk: *chunk_mod.Chunk, objects: *std.ArrayList(*object_mod.Obj)) Compiler {
        const result = Compiler{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner_mod.Scanner.init(source),
            .had_error = false,
            .panic_mode = false,
            .chunk = chunk,
            .objects = objects,
        };

        return result;
    }

    pub fn compile(self: *Compiler, allocator: std.mem.Allocator) bool {
        self.advance();
        self.expression(allocator) catch @panic("Error allocating memory during parsing");
        self.consume(.TOKEN_EOF, "Expected end of expression.");
        self.endCompiler(allocator) catch @panic("Could not write EOF to chunk");
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

    fn binary(self: *Compiler, allocator: std.mem.Allocator) !void {
        const operator_type = self.previous.t;
        const rule = getRule(operator_type);
        try self.parsePrecedence(allocator, nextPrecedence(rule.precedence));

        const opcode: chunk_mod.OpCode = switch (operator_type) {
            .TOKEN_PLUS => .OP_ADD,
            .TOKEN_MINUS => .OP_SUBTRACT,
            .TOKEN_STAR => .OP_MULTIPLY,
            .TOKEN_SLASH => .OP_DIVIDE,
            else => unreachable,
        };

        try self.emitByte(allocator, @intFromEnum(opcode));
    }

    fn literal(self: *Compiler, allocator: std.mem.Allocator) !void {
        const opcode: chunk_mod.OpCode = switch (self.previous.t) {
            .TOKEN_FALSE => .OP_FALSE,
            .TOKEN_NIL => .OP_NIL,
            .TOKEN_TRUE => .OP_TRUE,
            else => unreachable,
        };

        try self.emitByte(allocator, @intFromEnum(opcode));
    }

    fn expression(self: *Compiler, allocator: std.mem.Allocator) !void {
        try self.parsePrecedence(allocator, .Assignment);
    }
    fn grouping(self: *Compiler, allocator: std.mem.Allocator) !void {
        try self.expression(allocator);
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn number(self: *Compiler, allocator: std.mem.Allocator) !void {
        const value = try std.fmt.parseFloat(f64, self.previous.lexeme);
        try self.emitConstant(allocator, .{ .double = value });
    }

    fn string(self: *Compiler, allocator: std.mem.Allocator) !void {
        const len = self.previous.lexeme.len;
        const str = self.previous.lexeme[1 .. len - 1];
        const ptr = try allocator.dupe(u8, str);
        var obj = try allocator.create(object_mod.ObjString);
        obj.* = object_mod.ObjString{ .chars = ptr.ptr, .length = ptr.len, .obj = object_mod.Obj{ .obj_type = .String } };
        try self.objects.append(allocator, obj.asObj());
        try self.emitConstant(allocator, .{ .object = obj.asObj() });
    }

    fn unary(self: *Compiler, allocator: std.mem.Allocator) !void {
        const operator_type = self.previous.t;

        try self.parsePrecedence(allocator, .Unary);

        const opcode: chunk_mod.OpCode = switch (operator_type) {
            .TOKEN_BANG => .OP_NOT,
            .TOKEN_MINUS => .OP_NEGATE,
            else => unreachable,
        };

        try self.emitByte(allocator, @intFromEnum(opcode));
    }

    fn parsePrecedence(self: *Compiler, allocator: std.mem.Allocator, precedence: Precedence) !void {
        self.advance();
        const prefixRule = getRule(self.previous.t).prefix orelse {
            self.errorAtPrev("Expect expression.");
            return;
        };

        try prefixRule(self, allocator);

        while (precedenceLessOrEqual(precedence, getRule(self.current.t).precedence)) {
            self.advance();
            const infixRule = getRule(self.previous.t).infix orelse unreachable;
            try infixRule(self, allocator);
        }
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

    var objects = std.ArrayList(*object_mod.Obj).empty;
    defer objects.deinit(allocator);

    var compiler = Compiler.init("!", &chunk, &objects);
    compiler.advance();

    try std.testing.expectEqual(.TOKEN_BANG, compiler.current.t);
}

test "advance the thing twice, get a previous" {
    const allocator = std.testing.allocator;

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    var objects = std.ArrayList(*object_mod.Obj).empty;
    defer objects.deinit(allocator);

    var compiler = Compiler.init("! =", &chunk, &objects);
    compiler.advance();
    compiler.advance();

    try std.testing.expectEqual(.TOKEN_BANG, compiler.previous.t);
}

test "write a constant to the chunk pls" {
    const allocator = std.testing.allocator;

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    var objects = std.ArrayList(*object_mod.Obj).empty;
    defer objects.deinit(allocator);

    {
        var compiler = Compiler.init("42", &chunk, &objects);
        compiler.advance();
        compiler.advance();
        try compiler.number(allocator);
    }

    try std.testing.expectEqual(@intFromEnum(chunk_mod.OpCode.OP_CONSTANT), chunk.code.items[0]);
    try std.testing.expectEqual(0, chunk.code.items[1]);
    try std.testing.expectEqual(value_mod.Value{ .double = 42 }, chunk.constants.items[0]);
}

test "write a string to the chunk pls" {
    const allocator = std.testing.allocator;

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    var objects = std.ArrayList(*object_mod.Obj).empty;
    defer {
        objects.items[0].deinit(allocator);
        objects.deinit(allocator);
    }

    {
        var compiler = Compiler.init("\"foo\"", &chunk, &objects);
        compiler.advance();
        compiler.advance();
        try compiler.string(allocator);
    }

    try std.testing.expectEqual(@intFromEnum(chunk_mod.OpCode.OP_CONSTANT), chunk.code.items[0]);
}

test "infix addition" {
    const allocator = std.testing.allocator;

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    var objects = std.ArrayList(*object_mod.Obj).empty;
    defer objects.deinit(allocator);

    {
        var compiler = Compiler.init("2 + 3", &chunk, &objects);
        compiler.advance();
        try compiler.expression(allocator);
    }

    try std.testing.expectEqual(@intFromEnum(chunk_mod.OpCode.OP_CONSTANT), chunk.code.items[0]);
    try std.testing.expectEqual(0, chunk.code.items[1]);
    try std.testing.expectEqual(@intFromEnum(chunk_mod.OpCode.OP_CONSTANT), chunk.code.items[2]);
    try std.testing.expectEqual(1, chunk.code.items[3]);
    try std.testing.expectEqual(@intFromEnum(chunk_mod.OpCode.OP_ADD), chunk.code.items[4]);
}

test "unary negation" {
    const allocator = std.testing.allocator;

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    var objects = std.ArrayList(*object_mod.Obj).empty;
    defer objects.deinit(allocator);

    {
        var compiler = Compiler.init("-2", &chunk, &objects);
        compiler.advance();
        try compiler.expression(allocator);
    }

    try std.testing.expectEqual(@intFromEnum(chunk_mod.OpCode.OP_CONSTANT), chunk.code.items[0]);
    try std.testing.expectEqual(0, chunk.code.items[1]);
    try std.testing.expectEqual(@intFromEnum(chunk_mod.OpCode.OP_NEGATE), chunk.code.items[2]);
}
