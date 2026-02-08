const build_options = @import("build_options");
const std = @import("std");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const debug_mod = @import("debug.zig");
const compiler_mod = @import("compiler.zig");
const Compiler = compiler_mod.Compiler;
const object_mod = @import("object.zig");
const Obj = object_mod.Obj;
const ObjList = object_mod.ObjList;
const ObjString = object_mod.ObjString;

pub const InterpretError = error{
    InterpretCompileError,
    InterpretRuntimeError,
};

pub const VM = struct {
    chunk: ?*Chunk,
    ip: usize,
    stack: std.ArrayList(Value),
    objects: std.SinglyLinkedList,
    strings: std.StringHashMapUnmanaged([*]const u8),

    pub const init = VM{
        .chunk = null,
        .ip = 0,
        .stack = .empty,
        .objects = .{},
        .strings = .empty,
    };

    fn push(self: *VM, allocator: std.mem.Allocator, value: Value) !void {
        try self.stack.append(allocator, value);
    }

    fn pop(self: *VM) Value {
        return self.stack.pop() orelse unreachable;
    }

    fn peek(self: *VM, back: usize) Value {
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
        var chunk = Chunk.empty;
        defer chunk.deinit(allocator);

        var compiler = Compiler.init(source, &chunk, &self.objects, &self.strings);
        if (!compiler.compile(allocator)) {
            return InterpretError.InterpretCompileError;
        }

        self.chunk = &chunk;
        self.ip = 0;

        try self.run(allocator);
    }

    fn introspectInterpret(self: *VM, allocator: std.mem.Allocator, source: []const u8, chunk: *Chunk) !void {
        var compiler = Compiler.init(source, chunk, &self.objects, &self.strings);

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

    fn readConstant(self: *VM) Value {
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
        const result = Value{ .double = op(a, b) };
        try self.push(allocator, result);
    }

    fn ensureNumber(self: *VM, value: Value) !f64 {
        return switch (value) {
            .double => |val| val,
            else => {
                self.runtimeError("Operands must be numbers.", .{});
                return InterpretError.InterpretRuntimeError;
            },
        };
    }

    fn concatenate(self: *VM, allocator: std.mem.Allocator) !void {
        const a = self.pop().object.asObjString();
        const b = self.pop().object.asObjString();

        const str = try allocator.create(ObjString);
        const chars = try std.mem.concat(allocator, u8, &[_][]const u8{ b.chars[0..b.length], a.chars[0..a.length] });
        const interned = self.strings.get(chars);
        if (interned) |ptr| {
            str.* = ObjString{ .chars = ptr, .length = chars.len, .obj = Obj{ .obj_type = .String } };
            allocator.free(chars);
        } else {
            str.* = ObjString{ .chars = chars.ptr, .length = chars.len, .obj = Obj{ .obj_type = .String } };
            try self.strings.put(allocator, chars, chars.ptr);
        }
        self.objects.prepend(&(str.asObj()).node);
        try self.push(allocator, Value{ .object = str.asObj() });
    }

    pub fn step(self: *VM, allocator: std.mem.Allocator) !bool {
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
        const instruction: OpCode = @enumFromInt(self.readByte());
        switch (instruction) {
            .OP_RETURN => {
                return false;
            },
            .OP_CONSTANT => {
                const constant = self.readConstant();
                try self.push(allocator, constant);
            },
            .OP_NIL => try self.push(allocator, Value.nil),
            .OP_FALSE => try self.push(allocator, Value{ .bool = false }),
            .OP_TRUE => try self.push(allocator, Value{ .bool = true }),
            .OP_NEGATE => {
                const value = self.pop();
                const result = switch (value) {
                    .double => |double| Value{ .double = -double },
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
            .OP_NOT => try self.push(allocator, Value{ .bool = self.pop().isFalsish() }),
            .OP_EQUAL => {
                const b = self.pop();
                const a = self.pop();
                try self.push(allocator, Value{ .bool = a.isEqual(b) });
            },
            .OP_GREATER => {
                const b = try self.ensureNumber(self.pop());
                const a = try self.ensureNumber(self.pop());
                try self.push(allocator, Value{ .bool = a > b });
            },
            .OP_LESS => {
                const b = try self.ensureNumber(self.pop());
                const a = try self.ensureNumber(self.pop());
                try self.push(allocator, Value{ .bool = a < b });
            },
            .OP_PRINT => {
                self.pop().print();
                std.debug.print("\n", .{});
            },
            .OP_POP => {
                _ = self.pop();
            },
        }
        return true;
    }

    pub fn run(self: *VM, allocator: std.mem.Allocator) !void {
        while (try self.step(allocator)) {}
    }

    pub fn deinit(self: *VM, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
        var object_iterator = self.objects.first;
        while (object_iterator) |node| {
            const next = node.next;
            const obj: *Obj = @fieldParentPtr("node", node);
            obj.deinit(allocator);
            object_iterator = next;
        }
        var string_iterator = self.strings.iterator();
        while (string_iterator.next()) |entry| {
            const slice = entry.value_ptr.*[0..entry.key_ptr.len];
            allocator.free(slice);
        }
        self.strings.deinit(allocator);
    }
};

test "expressions need semi-colons" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try std.testing.expectError(InterpretError.InterpretCompileError, vm.introspectInterpret(allocator, "1+2", &chunk));
}

test "addition" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "1+2;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 1 }, vm.stack.items[0]);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 2 }, vm.stack.items[1]);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 3 }, vm.stack.items[0]);
    try std.testing.expect(try vm.step(allocator));
}

test "multiplication" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "7 * 5;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 7 }, vm.stack.items[0]);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 5 }, vm.stack.items[1]);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 35 }, vm.stack.items[0]);
    try std.testing.expect(try vm.step(allocator));
}

test "grouping" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "(3 + 2) * 2;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 3 }, vm.stack.items[0]);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 2 }, vm.stack.items[1]);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 5 }, vm.stack.items[0]);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 2 }, vm.stack.items[1]);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 10 }, vm.stack.items[0]);
    try std.testing.expect(try vm.step(allocator));
}

test "precedence" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "3 + 2 * 2;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 3 }, vm.stack.items[0]);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 2 }, vm.stack.items[1]);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 4 }, vm.stack.items[1]);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .double = 7 }, vm.stack.items[0]);
    try std.testing.expect(try vm.step(allocator));
}

test "negative false is an error" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "- false;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(Value{ .bool = false }, vm.stack.items[0]);
    const result = vm.step(allocator);
    try std.testing.expectError(InterpretError.InterpretRuntimeError, result);
}

test "3 + true is an error" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "3 + true;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    const result = vm.step(allocator);
    try std.testing.expectError(InterpretError.InterpretRuntimeError, result);
}

test "!nil is true" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "!nil;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(true, vm.stack.items[0].bool);
}

test "!5 is false" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "!5;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expectEqual(false, vm.stack.items[0].bool);
}

test "concatenate two strings" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "\"hello \" + \"world\";", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    const top: *ObjString = @ptrCast(@alignCast(vm.stack.items[0].object));
    try std.testing.expectEqualStrings("hello world", top.chars[0..top.length]);
}

test "what if two same strings" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "\"hello\" + \"hello\";", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    const top = vm.stack.items[0].object.asObjString();
    try std.testing.expectEqualStrings("hellohello", top.chars[0..top.length]);
}

test "5 == 5" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "5 == 5;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    const top = vm.stack.items[0].bool;
    try std.testing.expect(top);
}

test "\"hello\" == \"hello\"" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "\"hello\" == \"hello\";", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    const top = vm.stack.items[0].bool;
    try std.testing.expect(top);
}

test "6 != 5" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "6 != 5;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    const top = vm.stack.items[0].bool;
    try std.testing.expect(top);
}

test "6 > 5" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "6 > 5;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    const top = vm.stack.items[0].bool;
    try std.testing.expect(top);
}

test "string not equal 5" {
    const allocator = std.testing.allocator;

    var vm = VM.init;
    defer vm.deinit(allocator);

    var chunk = Chunk.empty;
    defer chunk.deinit(allocator);

    try vm.introspectInterpret(allocator, "\"hello\" == 5;", &chunk);
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    try std.testing.expect(try vm.step(allocator));
    const top = vm.stack.items[0].bool;
    try std.testing.expect(!top);
}
