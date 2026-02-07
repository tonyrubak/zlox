const std = @import("std");

pub const ObjType = enum {
    String,
};

pub const Obj = extern struct {
    obj_type: ObjType,

    pub fn deinit(self: *Obj, allocator: std.mem.Allocator) void {
        switch (self.obj_type) {
            .String => {
                const inner: *ObjString = @ptrCast(@alignCast(self));
                const slice = inner.chars[0..inner.length];
                allocator.free(slice);
                allocator.destroy(inner);
            },
        }
    }
};

pub const ObjString = extern struct {
    obj: Obj,
    length: usize,
    chars: [*]const u8,

    pub fn asObj(self: *ObjString) *Obj {
        return @ptrCast(self);
    }
};
