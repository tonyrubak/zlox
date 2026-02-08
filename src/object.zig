const std = @import("std");

pub const ObjType = enum {
    String,
};

pub const Obj = extern struct {
    obj_type: ObjType,

    pub fn deinit(self: *Obj, allocator: std.mem.Allocator) void {
        switch (self.obj_type) {
            .String => {
                const inner = self.asObjString();
                allocator.destroy(inner);
            },
        }
    }

    pub fn asObjString(self: *Obj) *ObjString {
        return switch (self.obj_type) {
            .String => @ptrCast(@alignCast(self)),
        };
    }
};

pub const ObjList = struct {
    data: *Obj,
    node: std.SinglyLinkedList.Node = .{},
};

pub const ObjString = extern struct {
    obj: Obj,
    length: usize,
    chars: [*]const u8,

    pub fn asObj(self: *ObjString) *Obj {
        return @ptrCast(self);
    }
};
