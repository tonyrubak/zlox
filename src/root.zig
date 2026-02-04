//! By convention, root.zig is the root source file when making a library.
const std = @import("std");

pub const chunk = @import("chunk.zig");
pub const debug = @import("debug.zig");
pub const vm = @import("vm.zig");

test {
    _ = @import("chunk.zig");
    _ = @import("vm.zig");
    _ = @import("scanner.zig");
    _ = @import("compiler.zig");
}
