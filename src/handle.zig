const std = @import("std");

pub fn Store(comptime T: type) type {
    return struct {
        values: std.ArrayListUnmanaged(T) = .{},

        const Self = @This();

        /// Invalidates getPtr
        pub fn add(self: *Self, value: T) !Handle(T) {
            const i = self.values.items.len;
            std.debug.assert(i < std.math.maxInt(u32));
            try self.values.append(value);
            return @intToEnum(Handle(T), @intCast(u32, i));
        }

        pub fn get(self: Self, handle: Handle(T)) T {
            return self.getPtr(handle).*;
        }

        /// Invalidated by add
        pub fn getPtr(self: Self, handle: Handle(T)) *T {
            std.debug.assert(handle != .invalid);
            return &self.values.items[@enumToInt(handle)];
        }
    };
}

pub fn Handle(comptime T: type) type {
    return enum(u32) {
        invalid = std.math.maxInt(u32),
        _,

        pub fn opt(self: Self) OptionalHandle(T) {
            std.debug.assert(self != .invalid);
            return @intToEnum(OptionalHandle(T), @enumToInt(self));
        }
    };
}

pub fn OptionalHandle(comptime T: type) type {
    return enum(u32) {
        none = std.math.maxInt(u32),
        _,

        const Self = @This();

        pub fn get(self: Self) ?Handle(T) {
            if (self == .none) {
                return null;
            } else {
                return @intToEnum(Handle(T), @enumToInt(self));
            }
        }
    };
}
