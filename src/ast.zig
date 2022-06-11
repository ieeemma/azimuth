const std = @import("std");

const handle = @import("handle.zig");
const Handle = handle.Handle;
const OptionalHandle = handle.OptionalHandle;
const Store = handle.Store;

pub const Expr = union(enum) {
    func: Func,
    call: Call,
    block: []Handle(Stmt),
    @"if": If,

    symbol: []const u8,
    int: i64,
    float: f64,

    pub const Func = struct {
        // TODO: type annotations!
        args: []const []const u8,
        body: Handle(Expr),
    };

    pub const Call = struct {
        target: Handle(Expr),
        args: []Handle(Expr),
    };

    pub const If = struct {
        cond: Handle(Expr),
        body: Handle(Expr),
        @"else": OptionalHandle(Expr),
    };
};

pub const Stmt = union(enum) {
    let: Let,
    brk: void,
    ret: Return,
    expr: Handle(Expr),

    pub const Let = struct {
        // TODO: variable unpacking/pattern matching
        name: []const u8,
        value: Handle(Expr),
    };

    pub const Return = struct {
        value: OptionalHandle(Expr),
    };
};

pub const Toplevel = union(enum) {
    let: Stmt.Let,
    // TODO: type declarations
};

pub const SyntaxTree = struct {
    toplevels: std.ArrayListUnmanaged(Toplevel),
    exprs: Store(Expr) = .{},
    stmts: Store(Stmt) = .{},
};
