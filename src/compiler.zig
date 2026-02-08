const std = @import("std");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const scanner_mod = @import("scanner.zig");
const Scanner = scanner_mod.Scanner;
const Token = scanner_mod.Token;
const TokenType = scanner_mod.TokenType;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const object_mod = @import("object.zig");
const Obj = object_mod.Obj;
const ObjList = object_mod.ObjList;
const ObjString = object_mod.ObjString;

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

fn getRule(token_type: TokenType) ParseRule {
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
        .TOKEN_BANG_EQUAL => .{ .prefix = null, .infix = Compiler.binary, .precedence = .Equality },
        .TOKEN_EQUAL => .{ .prefix = null, .infix = null, .precedence = .None },
        .TOKEN_EQUAL_EQUAL => .{ .prefix = null, .infix = Compiler.binary, .precedence = .Equality },
        .TOKEN_GREATER => .{ .prefix = null, .infix = Compiler.binary, .precedence = .Comparison },
        .TOKEN_GREATER_EQUAL => .{ .prefix = null, .infix = Compiler.binary, .precedence = .Comparison },
        .TOKEN_LESS => .{ .prefix = null, .infix = Compiler.binary, .precedence = .Comparison },
        .TOKEN_LESS_EQUAL => .{ .prefix = null, .infix = Compiler.binary, .precedence = .Comparison },
        .TOKEN_IDENTIFIER => .{ .prefix = null, .infix = Compiler.binary, .precedence = .Comparison },
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
    current: Token,
    previous: Token,
    scanner: Scanner,
    had_error: bool,
    panic_mode: bool,
    chunk: *Chunk,
    objects: *std.SinglyLinkedList,
    strings: *std.StringHashMapUnmanaged([*]const u8),

    pub fn init(source: []const u8, chunk: *Chunk, objects: *std.SinglyLinkedList, strings: *std.StringHashMapUnmanaged([*]const u8)) Compiler {
        const result = Compiler{
            .current = undefined,
            .previous = undefined,
            .scanner = Scanner.init(source),
            .had_error = false,
            .panic_mode = false,
            .chunk = chunk,
            .objects = objects,
            .strings = strings,
        };

        return result;
    }

    pub fn compile(self: *Compiler, allocator: std.mem.Allocator) bool {
        self.advance();
        while (!self.match(.TOKEN_EOF)) {
            self.declaration(allocator) catch @panic("Error allocating memory during parsing");
        }
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

    fn consume(self: *Compiler, token_type: TokenType, message: []const u8) void {
        if (self.current.t == token_type) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn check(self: *const Compiler, token_type: TokenType) bool {
        return self.current.t == token_type;
    }

    fn match(self: *Compiler, token_type: TokenType) bool {
        if (!self.check(token_type)) return false;
        self.advance();
        return true;
    }

    fn emitByte(self: *Compiler, allocator: std.mem.Allocator, byte: u8) !void {
        try self.chunk.write(allocator, byte, self.previous.line);
    }

    fn emitBytes(self: *Compiler, allocator: std.mem.Allocator, byte1: u8, byte2: u8) !void {
        try self.emitByte(allocator, byte1);
        try self.emitByte(allocator, byte2);
    }

    fn emitReturn(self: *Compiler, allocator: std.mem.Allocator) !void {
        try self.emitOp(allocator, OpCode.OP_RETURN);
    }

    fn makeConstant(self: *Compiler, allocator: std.mem.Allocator, value: Value) !u8 {
        const constant = try self.chunk.addConstant(allocator, value);
        if (constant > std.math.maxInt(u8)) {
            self.errorAtPrev("Too many constants in one chunk.");
            return 0;
        }

        return constant;
    }

    fn emitConstant(self: *Compiler, allocator: std.mem.Allocator, value: Value) !void {
        const constant = try self.makeConstant(allocator, value);
        try self.emitBytes(allocator, @intFromEnum(OpCode.OP_CONSTANT), constant);
    }

    fn emitOp(self: *Compiler, allocator: std.mem.Allocator, opcode: OpCode) !void {
        try self.emitByte(allocator, @intFromEnum(opcode));
    }

    fn emitOps(self: *Compiler, allocator: std.mem.Allocator, op1: OpCode, op2: OpCode) !void {
        try self.emitBytes(allocator, @intFromEnum(op1), @intFromEnum(op2));
    }

    fn endCompiler(self: *Compiler, allocator: std.mem.Allocator) !void {
        try self.emitReturn(allocator);
    }

    fn binary(self: *Compiler, allocator: std.mem.Allocator) !void {
        const operator_type = self.previous.t;
        const rule = getRule(operator_type);
        try self.parsePrecedence(allocator, nextPrecedence(rule.precedence));

        try switch (operator_type) {
            .TOKEN_PLUS => self.emitOp(allocator, .OP_ADD),
            .TOKEN_MINUS => self.emitOp(allocator, .OP_SUBTRACT),
            .TOKEN_STAR => self.emitOp(allocator, .OP_MULTIPLY),
            .TOKEN_SLASH => self.emitOp(allocator, .OP_DIVIDE),
            .TOKEN_BANG_EQUAL => self.emitOps(allocator, .OP_EQUAL, .OP_NOT),
            .TOKEN_EQUAL_EQUAL => self.emitOp(allocator, .OP_EQUAL),
            .TOKEN_GREATER => self.emitOp(allocator, .OP_GREATER),
            .TOKEN_GREATER_EQUAL => self.emitOps(allocator, .OP_LESS, .OP_NOT),
            .TOKEN_LESS => self.emitOp(allocator, .OP_LESS),
            .TOKEN_LESS_EQUAL => self.emitOps(allocator, .OP_GREATER, .OP_NOT),
            else => unreachable,
        };
    }

    fn literal(self: *Compiler, allocator: std.mem.Allocator) !void {
        const opcode: OpCode = switch (self.previous.t) {
            .TOKEN_FALSE => .OP_FALSE,
            .TOKEN_NIL => .OP_NIL,
            .TOKEN_TRUE => .OP_TRUE,
            else => unreachable,
        };

        try self.emitOp(allocator, opcode);
    }

    fn expression(self: *Compiler, allocator: std.mem.Allocator) !void {
        try self.parsePrecedence(allocator, .Assignment);
    }

    fn expressionStatement(self: *Compiler, allocator: std.mem.Allocator) !void {
        try self.expression(allocator);
        self.consume(.TOKEN_SEMICOLON, "Expect ';' after expression.");
        try self.emitOp(allocator, .OP_POP);
    }

    fn printStatement(self: *Compiler, allocator: std.mem.Allocator) !void {
        try self.expression(allocator);
        self.consume(.TOKEN_SEMICOLON, "Expect ';' after value.");
        try self.emitOp(allocator, .OP_PRINT);
    }

    fn synchronize(self: *Compiler) void {
        self.panic_mode = false;

        while (self.current.t != .TOKEN_EOF) {
            if (self.previous.t == .TOKEN_SEMICOLON) return;

            switch (self.current.t) {
                .TOKEN_CLASS, .TOKEN_FUN, .TOKEN_VAR, .TOKEN_FOR, .TOKEN_IF, .TOKEN_WHILE, .TOKEN_PRINT, .TOKEN_RETURN => return,
                else => {},
            }

            self.advance();
        }
    }

    fn declaration(self: *Compiler, allocator: std.mem.Allocator) !void {
        try self.statement(allocator);

        if (self.panic_mode) self.synchronize();
    }

    fn statement(self: *Compiler, allocator: std.mem.Allocator) !void {
        if (self.match(.TOKEN_PRINT)) {
            try self.printStatement(allocator);
        } else {
            try self.expressionStatement(allocator);
        }
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
        const ptr = self.strings.get(str) orelse (try allocator.dupe(u8, str)).ptr;
        try self.strings.put(allocator, str, ptr);
        var obj = try allocator.create(ObjString);
        obj.* = ObjString{ .chars = ptr, .length = str.len, .obj = Obj{ .obj_type = .String } };
        self.objects.prepend(&(obj.asObj()).node);
        try self.emitConstant(allocator, .{ .object = obj.asObj() });
    }

    fn unary(self: *Compiler, allocator: std.mem.Allocator) !void {
        const operator_type = self.previous.t;

        try self.parsePrecedence(allocator, .Unary);

        const opcode: OpCode = switch (operator_type) {
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

    fn errorAt(self: *Compiler, token: *const Token, message: []const u8) void {
        if (self.panic_mode) return;
        self.panic_mode = true;
        std.debug.print("[line {d}] Error", .{token.line});

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

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    var objects: std.SinglyLinkedList = .{};

    var strings = std.StringHashMapUnmanaged([*]const u8).empty;
    defer strings.deinit(allocator);

    var compiler = Compiler.init("!", &chunk, &objects, &strings);
    compiler.advance();

    try std.testing.expectEqual(.TOKEN_BANG, compiler.current.t);
}

test "advance the thing twice, get a previous" {
    const allocator = std.testing.allocator;

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    var objects: std.SinglyLinkedList = .{};

    var strings = std.StringHashMapUnmanaged([*]const u8).empty;
    defer strings.deinit(allocator);

    var compiler = Compiler.init("! =", &chunk, &objects, &strings);
    compiler.advance();
    compiler.advance();

    try std.testing.expectEqual(.TOKEN_BANG, compiler.previous.t);
}

test "write a constant to the chunk pls" {
    const allocator = std.testing.allocator;

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    var objects: std.SinglyLinkedList = .{};

    var strings = std.StringHashMapUnmanaged([*]const u8).empty;
    defer strings.deinit(allocator);

    {
        var compiler = Compiler.init("42", &chunk, &objects, &strings);
        compiler.advance();
        compiler.advance();
        try compiler.number(allocator);
    }

    try std.testing.expectEqual(@intFromEnum(OpCode.OP_CONSTANT), chunk.code.items[0]);
    try std.testing.expectEqual(0, chunk.code.items[1]);
    try std.testing.expectEqual(Value{ .double = 42 }, chunk.constants.items[0]);
}

test "write a string to the chunk pls" {
    const allocator = std.testing.allocator;

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    var objects: std.SinglyLinkedList = .{};
    defer {
        var object_iterator = objects.first;
        while (object_iterator) |node| {
            const next = node.next;
            const obj: *Obj = @fieldParentPtr("node", node);
            obj.deinit(allocator);
            object_iterator = next;
        }
    }

    var strings = std.StringHashMapUnmanaged([*]const u8).empty;
    defer {
        var it = strings.iterator();
        while (it.next()) |entry| {
            const slice = entry.value_ptr.*[0..entry.key_ptr.len];
            allocator.free(slice);
        }
        strings.deinit(allocator);
    }

    {
        var compiler = Compiler.init("\"foo\"", &chunk, &objects, &strings);
        compiler.advance();
        compiler.advance();
        try compiler.string(allocator);
    }

    try std.testing.expectEqual(@intFromEnum(OpCode.OP_CONSTANT), chunk.code.items[0]);
}

test "infix addition" {
    const allocator = std.testing.allocator;

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    var objects: std.SinglyLinkedList = .{};

    var strings = std.StringHashMapUnmanaged([*]const u8).empty;
    defer strings.deinit(allocator);

    {
        var compiler = Compiler.init("2 + 3", &chunk, &objects, &strings);
        compiler.advance();
        try compiler.expression(allocator);
    }

    try std.testing.expectEqual(@intFromEnum(OpCode.OP_CONSTANT), chunk.code.items[0]);
    try std.testing.expectEqual(0, chunk.code.items[1]);
    try std.testing.expectEqual(@intFromEnum(OpCode.OP_CONSTANT), chunk.code.items[2]);
    try std.testing.expectEqual(1, chunk.code.items[3]);
    try std.testing.expectEqual(@intFromEnum(OpCode.OP_ADD), chunk.code.items[4]);
}

test "unary negation" {
    const allocator = std.testing.allocator;

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    var objects: std.SinglyLinkedList = .{};

    var strings = std.StringHashMapUnmanaged([*]const u8).empty;
    defer strings.deinit(allocator);

    {
        var compiler = Compiler.init("-2", &chunk, &objects, &strings);
        compiler.advance();
        try compiler.expression(allocator);
    }

    try std.testing.expectEqual(@intFromEnum(OpCode.OP_CONSTANT), chunk.code.items[0]);
    try std.testing.expectEqual(0, chunk.code.items[1]);
    try std.testing.expectEqual(@intFromEnum(OpCode.OP_NEGATE), chunk.code.items[2]);
}
