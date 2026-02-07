const build_options = @import("build_options");
const std = @import("std");
const chunk_mod = @import("chunk.zig");
const value_mod = @import("value.zig");
const debug_mod = @import("debug.zig");
const compiler_mod = @import("compiler.zig");
const object_mod = @import("object.zig");

pub const InterpretError = error{
    InterpretCompileError,
    InterpretRuntimeError,
};

pub const VM = struct {
    chunk: ?*chunk_mod.Chunk,
    ip: usize,
    stack: std.ArrayList(value_mod.Value),
    trace_execution: bool,
    objects: std.ArrayList(*object_mod.Obj),

    pub const init = VM{
        .chunk = null,
        .ip = 0,
        .stack = .empty,
        .trace_execution = true,
        .objects = .empty,
    };

    fn push(self: *VM, allocator: std.mem.Allocator, value: value_mod.Value) !void {
        try self.stack.append(allocator, value);
    }

    fn pop(self: *VM) value_mod.Value {
        return self.stack.pop() orelse unreachable;
    }

    fn peek(self: *VM, back: usize) value_mod.Value {
        return self.stack.items[self.stack.items.len - back - 1];
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});

        const line = self.chunk.?.lines.items[self.ip];

        std.debug.print("[line {d}] in script\n", .{line});
        self.stack.clearRetainingCapacity();
    }

    pub fn interpret(self: *VM, allocator: std.mem.Allocator, source: []const u8) !void {
        var chunk = chunk_mod.Chunk.empty;
        defer chunk.deinit(allocator);

        var compiler = compiler_mod.Compiler.init(source, &chunk, &self.objects);
        if (!compiler.compile(allocator)) {
            return InterpretError.InterpretCompileError;
        }

        self.chunk = &chunk;
        self.ip = 0;

        self.run(allocator);
    }

    fn introspectInterpret(self: *VM, allocator: std.mem.Allocator, source: []const u8, chunk: *chunk_mod.Chunk) !void {
        var compiler = compiler_mod.Compiler.init(source, chunk, &self.objects);

        if (!compiler.compile(allocator)) {
            return InterpretError.InterpretCompileError;
        }

        self.chunk = chunk;
        self.ip = 0;
    }

    fn readByte(self: *VM) u8 {
        const result = self.chunk.?.code.items[self.ip];
        self.ip += 1;
        return result;
    }

    fn readConstant(self: *VM) value_mod.Value {
        const offset = self.readByte();
        return self.chunk.?.constants.items[offset];
    }

    fn add(a: f64, b: f64) f64 {
        return a + b;
    }
    fn sub(a: f64, b: f64) f64 {
        return a - b;
    }
    fn mul(a: f64, b: f64) f64 {
        return a * b;
    }
    fn div(a: f64, b: f64) f64 {
        return a / b;
    }

    fn binaryOp(self: *VM, allocator: std.mem.Allocator, comptime op: fn (f64, f64) f64) !void {
        const b = try self.ensureNumber(self.pop());
        const a = try self.ensureNumber(self.pop());
        const result = value_mod.Value{ .double = op(a, b) };
        try self.push(allocator, result);
    }

    fn ensureNumber(self: *VM, value: value_mod.Value) !f64 {
        return switch (value) {
            .double => |val| val,
            else => {
                self.runtimeError("Operands must be numbers.", .{});
                return InterpretError.InterpretRuntimeError;
            },
        };
    }

    fn concatenate(self: *VM, allocator: std.mem.Allocator) !void {
        const a: *object_mod.ObjString = @ptrCast(@alignCast(self.pop().object));
        const b: *object_mod.ObjString = @ptrCast(@alignCast(self.pop().object));

        const str = try allocator.create(object_mod.ObjString);
        const chars = try std.mem.concat(allocator, u8, &[_][]const u8{ b.chars[0..b.length], a.chars[0..a.length] });
        str.* = object_mod.ObjString{ .chars = chars.ptr, .length = chars.len, .obj = object_mod.Obj{ .obj_type = .String } };
        try self.objects.append(allocator, str.asObj());
        try self.push(allocator, value_mod.Value{ .object = str.asObj() });
    }

    pub fn step(self: *VM, allocator: std.mem.Allocator) !void {
        if (comptime build_options.trace) {
            std.debug.print("          ", .{});
            for (self.stack.items) |item| {
                std.debug.print("[ ", .{});
                item.print();
                std.debug.print(" ]", .{});
            }
            std.debug.print("\n", .{});
            _ = debug_mod.disassembleInstruction(self.chunk.?, self.ip);
        }
        const instruction: chunk_mod.OpCode = @enumFromInt(self.readByte());
        switch (instruction) {
            .OP_RETURN => {
                const value = self.pop();
                value.print();
                std.debug.print("\n", .{});
                return;
            },
            .OP_CONSTANT => {
                const constant = self.readConstant();
                try self.push(allocator, constant);
            },
            .OP_NIL => try self.push(allocator, value_mod.Value.nil),
            .OP_FALSE => try self.push(allocator, value_mod.Value{ .bool = false }),
            .OP_TRUE => try self.push(allocator, value_mod.Value{ .bool = true }),
            .OP_NEGATE => {
                const value = self.pop();
                const result = switch (value) {
                    .double => |double| value_mod.Value{ .double = -double },
                    else => {
                        self.runtimeError("Operand must be a number.", .{});
                        return InterpretError.InterpretRuntimeError;
                    },
                };
                try self.push(allocator, result);
            },
            .OP_ADD => {
                const top = self.peek(0);
                const next = self.peek(1);

                if (top.isObjType(.String) and next.isObjType(.String)) {
                    try self.concatenate(allocator);
                } else if (top.isNumber() and next.isNumber()) {
                    try self.binaryOp(allocator, add);
                } else {
                    self.runtimeError("Operands must be two numbers or two strings", .{});
                    return InterpretError.InterpretRuntimeError;
                }
            },
            .OP_SUBTRACT => try self.binaryOp(allocator, sub),
            .OP_MULTIPLY => try self.binaryOp(allocator, mul),
            .OP_DIVIDE => try self.binaryOp(allocator, div),
            .OP_NOT => try self.push(allocator, value_mod.Value{ .bool = self.pop().isFalsish() }),
        }
    }

    pub fn run(self: *VM, allocator: std.mem.Allocator) !void {
        while (true) {
            self.step(allocator);
        }
    }

    pub fn deinit(self: *VM, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
        for (self.objects.items) |object| {
            object.deinit(allocator);
        }
        self.objects.deinit(allocator);
    }
};

test "addition" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "1+2", &chunk);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 1 }, vm.stack.items[0]);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 2 }, vm.stack.items[1]);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 3 }, vm.stack.items[0]);
    try vm.step(allocator);
}

test "multiplication" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "7 * 5", &chunk);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 7 }, vm.stack.items[0]);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 5 }, vm.stack.items[1]);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 35 }, vm.stack.items[0]);
    try vm.step(allocator);
}

test "grouping" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "(3 + 2) * 2", &chunk);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 3 }, vm.stack.items[0]);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 2 }, vm.stack.items[1]);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 5 }, vm.stack.items[0]);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 2 }, vm.stack.items[1]);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 10 }, vm.stack.items[0]);
    try vm.step(allocator);
}

test "precedence" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "3 + 2 * 2", &chunk);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 3 }, vm.stack.items[0]);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 2 }, vm.stack.items[1]);
    try vm.step(allocator);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 4 }, vm.stack.items[1]);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .double = 7 }, vm.stack.items[0]);
    try vm.step(allocator);
}

test "negative false is an error" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "- false", &chunk);
    try vm.step(allocator);
    try std.testing.expectEqual(value_mod.Value{ .bool = false }, vm.stack.items[0]);
    const result = vm.step(allocator);
    try std.testing.expectError(InterpretError.InterpretRuntimeError, result);
}

test "3 + true is an error" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "3 + true", &chunk);
    try vm.step(allocator);
    try vm.step(allocator);
    const result = vm.step(allocator);
    try std.testing.expectError(InterpretError.InterpretRuntimeError, result);
}

test "!nil is true" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "!nil", &chunk);
    try vm.step(allocator);
    try vm.step(allocator);
    try std.testing.expectEqual(true, vm.stack.items[0].bool);
}

test "!5 is false" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "!5", &chunk);
    try vm.step(allocator);
    try vm.step(allocator);
    try std.testing.expectEqual(false, vm.stack.items[0].bool);
}

test "concatenate two strings" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = chunk_mod.Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "\"hello \" + \"world\"", &chunk);
    try vm.step(allocator);
    try vm.step(allocator);
    try vm.step(allocator);
    const top: *object_mod.ObjString = @ptrCast(@alignCast(vm.stack.items[0].object));
    try std.testing.expectEqualStrings("hello world", top.chars[0..top.length]);
}
