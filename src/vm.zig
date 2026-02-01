const std = @import("std");
const chunk_mod = @import("chunk.zig");
const value_mod = @import("value.zig");

pub const InterpretError = error{
    InterpretCompileError,
    InterpretRuntimeError,
};

pub const VM = struct {
    chunk: ?*chunk_mod.Chunk,
    ip: ?[*]u8,

    pub const init = VM{
        .chunk = null,
        .ip = null,
    };

    pub fn interpret(self: *VM, chunk_ptr: *chunk_mod.Chunk) !void {
        self.chunk = chunk_ptr;
        self.ip = self.chunk.?.code.items.ptr;
        return self.run();
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

    pub fn run(self: *VM) !void {
        while (true) {
            const instruction: chunk_mod.OpCode = @enumFromInt(self.readByte());
            switch (instruction) {
                .OP_RETURN => return,
                .OP_CONSTANT => {
                    const constant = self.readConstant();
                    constant.print();
                    std.debug.print("\n", .{});
                },
            }
        }
    }

    pub fn deinit(self: *VM, allocator: std.mem.Allocator) void {
        _ = allocator;
        _ = self;
    }
};
