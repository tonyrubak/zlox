const std = @import("std");

pub const ObjType = enum {
    String,
};

pub const Obj = struct {
    obj_type: ObjType,
    node: std.SinglyLinkedList.Node = .{},

    pub fn deinit(self: *Obj, allocator: std.mem.Allocator) void {
        switch (self.obj_type) {
            .String => allocator.destroy(self.asObjString()),
        }
    }

    pub fn asObjString(self: *Obj) *ObjString {
        return switch (self.obj_type) {
            .String => @fieldParentPtr("obj", self),
        };
    }
};

pub const ObjString = struct {
    obj: Obj,
    length: usize,
    chars: [*]const u8,

    pub fn asObj(self: *ObjString) *Obj {
        return @ptrCast(self);
    }
};
