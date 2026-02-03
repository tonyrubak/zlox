const build_options = @import("build_options");
const std = @import("std");
const chunk_mod = @import("chunk.zig");
const value_mod = @import("value.zig");
const debug_mod = @import("debug.zig");
const compiler_mod = @import("compiler.zig");

pub const InterpretError = error{
    InterpretCompileError,
    InterpretRuntimeError,
};

pub const VM = struct {
    chunk: ?*chunk_mod.Chunk,
    ip: ?[*]u8,
    stack: std.ArrayList(value_mod.Value),
    trace_execution: bool,

    pub const init = VM{
        .chunk = null,
        .ip = null,
        .stack = std.ArrayList(value_mod.Value).empty,
        .trace_execution = true,
    };

    fn push(self: *VM, allocator: std.mem.Allocator, value: value_mod.Value) !void {
        try self.stack.append(allocator, value);
    }

    fn pop(self: *VM) value_mod.Value {
        return self.stack.pop() orelse unreachable;
    }

    pub fn interpret(_: *VM, _: std.mem.Allocator, source: []const u8) void {
        compiler_mod.compile(source);
    }

    fn readByte(self: *VM) u8 {
        if (self.ip == null) unreachable;
        const result = self.ip.?[0];
        self.ip.? += 1;
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
        const b = self.pop();
        const a = self.pop();
        const result = value_mod.Value{ .double = op(a.double, b.double) };
        try self.push(allocator, result);
    }

    pub fn run(self: *VM, allocator: std.mem.Allocator) !void {
        while (true) {
            if (comptime build_options.trace) {
                std.debug.print("          ", .{});
                for (self.stack.items) |item| {
                    std.debug.print("[ ", .{});
                    item.print();
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});
                _ = debug_mod.disassembleInstruction(self.chunk.?, (self.ip.? - self.chunk.?.code.items.ptr));
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
                .OP_NEGATE => {
                    const value = self.pop();
                    const result = switch (value) {
                        .double => |double| value_mod.Value{ .double = -double },
                    };
                    try self.push(allocator, result);
                },
                .OP_ADD => try self.binaryOp(allocator, add),
                .OP_SUBTRACT => try self.binaryOp(allocator, sub),
                .OP_MULTIPLY => try self.binaryOp(allocator, mul),
                .OP_DIVIDE => try self.binaryOp(allocator, div),
            }
        }
    }

    pub fn deinit(self: *VM, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
    }
};
