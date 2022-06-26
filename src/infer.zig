const std = @import("std");

const ast = @import("ast.zig");
const handle = @import("handle.zig");

/// Type variables are unique 'placeholder' types.
const TVar = enum(u64) { _ };

const TVarSet = std.AutoHashMapUnmanaged(TVar, void);

// TODO: This is a huge hack. To make the types work, these are
// singleton mutable types. Constructor types will never be modified.
var t_void = Type{ .con = "Void" };
var t_bool = Type{ .con = "Bool" };
var t_int = Type{ .con = "Int" };
var t_float = Type{ .con = "Float" };

/// A type can be:
///     - A function
///     - An application, eg `List a`
///     - A constructor, eg `List`
///     - A type variable, eg `a`
const Type = union(enum) {
    func: Func,
    app: [2]*Type,
    con: []const u8,
    tvar: TVar,

    const Func = struct {
        args: []*Type,
        ret: *Type,
    };

    fn free(self: *Type, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .func => |func| {
                for (func.args) |arg| arg.free(allocator);
                allocator.free(func.args);
                func.ret.free(allocator);
            },
            .app => |app| {
                app[0].free(allocator);
                app[1].free(allocator);
            },
            else => {},
        }
        allocator.destroy(self);
    }

    fn copy(self: Type, allocator: std.mem.Allocator) std.mem.Allocator.Error!*Type {
        const t = try allocator.create(Type);
        switch (self) {
            .func => |func| {
                const args = try allocator.alloc(*Type, func.args.len);
                for (func.args) |arg, i| {
                    args[i] = try arg.copy(allocator);
                }
                const ret = try func.ret.copy(allocator);
                t.* = .{ .func = .{ .args = args, .ret = ret } };
            },
            .app => |app| t.* = .{ .app = .{ try app[0].copy(allocator), try app[1].copy(allocator) } },
            .con => |con| t.* = .{ .con = con },
            .tvar => |tvar| t.* = .{ .tvar = tvar },
        }

        return t;
    }

    /// Find all of the free type variables in a type.
    fn freevars(self: Type, allocator: std.mem.Allocator, set: *TVarSet) std.mem.Allocator.Error!void {
        switch (self) {
            .func => |func| {
                for (func.args) |arg| try arg.freevars(allocator, set);
                try func.ret.freevars(allocator, set);
            },
            .app => |app| {
                try app[0].freevars(allocator, set);
                try app[1].freevars(allocator, set);
            },
            .con => {},
            .tvar => |tv| try set.put(allocator, tv, {}),
        }
    }

    /// Apply a substitution to a type by replacing each type variable with
    /// a corresponding type.
    fn subst(self: *Type, sub: *Sub, mono: ?*TVarSet) void {
        switch (self.*) {
            .func => |func| {
                for (func.args) |arg| arg.subst(sub, mono);
                func.ret.subst(sub, mono);
            },
            .app => |app| {
                app[0].subst(sub, mono);
                app[1].subst(sub, mono);
            },
            .con => {},
            .tvar => |tv| {
                if (mono == null or !mono.?.contains(tv)) {
                    if (sub.get(tv)) |t| self.* = t.*;
                }
            },
        }
    }

    /// Check if a type contains a given type variable.
    fn contains(self: Type, tv: TVar) bool {
        return switch (self) {
            .func => |func| {
                var c = false;
                for (func.args) |arg| c = c or arg.contains(tv);
                return c or func.ret.contains(tv);
            },
            .app => |app| app[0].contains(tv) or app[1].contains(tv),
            .con => false,
            .tvar => |tv2| tv == tv2,
        };
    }
};

/// A substitution is a mapping of type variables to replacement types.
const Sub = std.AutoHashMapUnmanaged(TVar, *Type);

/// A type scheme encodes universal quantification, such as `id : forall a. a -> a`.
/// It contains a set of monomorphic type variables and a wrapped type.
const Scheme = struct {
    mono: TVarSet,
    type: *Type,

    /// Find all of the free type variables in a scheme.
    fn freevars(self: Scheme, allocator: std.mem.Allocator, set: *TVarSet) !void {
        try self.type.freevars(allocator, set);
        // TODO: is it safe to remove from the global set here?
        var it = set.keyIterator();
        while (it.next()) |tv| {
            _ = set.remove(tv.*);
        }
    }

    /// Apply a substitution to the type within a type scheme.
    fn subst(self: Scheme, sub: Sub) void {
        self.type.subst(sub, self.mono);
    }
};

/// The environment records the types of each variable. It is a linked list of
/// scopes, each of which maps names to types.
/// It also contains the current functions return type, which can be null
/// when checking the top level.
const Env = struct {
    scope: Scope,
    ret: ?*Type,
    prev: ?*const Env,

    const Scope = std.StringHashMapUnmanaged(Scheme);

    /// Find all of the free type variables in the types in the environment.
    fn freevars(self: *const Env, allocator: std.mem.Allocator, set: *TVarSet) !void {
        var e = self;
        while (e.prev != null) : (e = e.prev.?) {
            var it = e.scope.valueIterator();
            while (it.next()) |t| try t.freevars(allocator, set);
        }
    }
};

pub const Infer = struct {
    allocator: std.mem.Allocator,
    tree: ast.SyntaxTree,
    sub: Sub = .{},
    tv_count: u64 = 0,

    pub fn init(allocator: std.mem.Allocator, tree: ast.SyntaxTree) Infer {
        return .{
            .allocator = allocator,
            .tree = tree,
        };
    }

    pub fn deinit(self: *Infer) void {
        self.env.map.deinit();
        self.sub.deinit();
    }

    /// Create a unique type variable.
    fn fresh(self: *Infer) !*Type {
        self.tv_count += 1;
        const tv = try self.allocator.create(Type);
        tv.* = .{ .tvar = @intToEnum(TVar, self.tv_count) };
        return tv;
    }

    /// Produce a scheme from a type and a set of known monomorphic type variables.
    fn generalize(self: Infer, t: *Type, mono: *const TVarSet) !Scheme {
        var fvs: TVarSet = .{};
        try t.freevars(self.allocator, &fvs);
        var it = mono.keyIterator();
        while (it.next()) |tv| {
            _ = fvs.remove(tv.*);
        }
        return Scheme{ .mono = fvs, .type = t };
    }

    /// Produce a type from a scheme by replacing every instance of each quantified
    /// type variable with a `fresh` variable in the contained type.
    fn instantiate(self: *Infer, s: Scheme) !*Type {
        var sub: Sub = .{};
        defer sub.deinit(self.allocator);

        var it = s.mono.keyIterator();
        while (it.next()) |tv| {
            try sub.put(self.allocator, tv.*, try self.fresh());
        }

        const t = try s.type.copy(self.allocator);
        t.subst(&sub, null);
        return t;
    }

    /// Helper function to apply a substitution to two types and then unify them.
    fn substAndUnify(self: Infer, a: *Type, b: *Type, sub: *Sub) anyerror!void {
        const a2 = try a.copy(self.allocator);
        defer a2.free(self.allocator);
        a2.subst(sub, null);

        const b2 = try b.copy(self.allocator);
        defer b2.free(self.allocator);
        b2.subst(sub, null);

        try self.unify(a2, b2, sub);
    }

    /// Find a substitution mapping one type to another. In any well-typed program,
    /// there is always such a substitution.
    fn unify(self: Infer, a: *Type, b: *Type, sub: *Sub) anyerror!void {
        if (a.* == .tvar and !b.contains(a.tvar)) {
            try sub.put(self.allocator, a.tvar, b);
        } else if (b.* == .tvar and !a.contains(b.tvar)) {
            try sub.put(self.allocator, b.tvar, a);
        } else if (a.* == .con and b.* == .con and std.mem.eql(u8, a.con, b.con)) {
            // TODO: store a hash on `con` to make this comparison faster
        } else if (a.* == .func and b.* == .func and a.func.args.len == b.func.args.len) {
            for (a.func.args) |arg, i| {
                try self.substAndUnify(arg, b.func.args[i], sub);
            }
            try self.substAndUnify(a.func.ret, b.func.ret, sub);
        } else if (a.* == .app and b.* == .app) {
            // TODO: is the order of this safe? Do i need to duplicate sub?
            // Ideally it would be:
            //     θ = unify τ τ'
            //     φ = unify (subst θ π) (subst θ π')
            //     merge θ φ
            try self.substAndUnify(a.app[0], b.app[0], sub);
            try self.substAndUnify(a.app[1], b.app[1], sub);
        } else {
            return error.UnificationError;
        }
    }

    inline fn getExpr(self: Infer, h: handle.Handle(ast.Expr)) ast.Expr {
        return self.tree.exprs.get(h);
    }

    inline fn getStmt(self: Infer, h: handle.Handle(ast.Stmt)) ast.Stmt {
        return self.tree.stmts.get(h);
    }

    /// Infer the type of an expression given the current environment.
    pub fn inferExpr(self: *Infer, expr: ast.Expr, env: *const Env) anyerror!*Type {
        switch (expr) {
            .func => |func| {
                const tv = try self.fresh();
                var new = Env{ .scope = .{}, .ret = tv, .prev = env };
                defer new.scope.deinit(self.allocator);

                const tvs = try self.allocator.alloc(*Type, func.args.len);
                for (func.args) |arg, i| {
                    const tv2 = try self.fresh();
                    tvs[i] = tv2;
                    try new.scope.put(
                        self.allocator,
                        arg,
                        .{ .mono = .{}, .type = tv2 },
                    );
                }

                const b = try self.inferExpr(self.getExpr(func.body), &new);
                try self.unify(b, tv, &self.sub);

                const t = try self.allocator.create(Type);
                t.* = .{ .func = .{ .args = tvs, .ret = b } };
                return t;
            },
            .call => |call| {
                const f1 = try self.inferExpr(self.getExpr(call.target), env);

                const tv = try self.fresh();
                const as = try self.allocator.alloc(*Type, call.args.len);
                defer {
                    for (as) |arg| arg.free(self.allocator);
                    self.allocator.free(as);
                }

                for (as) |*arg, i| {
                    arg.* = try self.inferExpr(self.getExpr(call.args[i]), env);
                }

                const f2 = try self.allocator.create(Type);
                defer self.allocator.destroy(f2);
                f2.* = .{ .func = .{ .args = as, .ret = tv } };

                try self.unify(f1, f2, &self.sub);
                return tv;
            },
            .block => |block| {
                // Check each stmt under a new scope
                var new = Env{ .scope = .{}, .ret = env.ret, .prev = env };
                defer new.scope.deinit(self.allocator);
                for (block) |stmt| {
                    try self.checkStmt(self.getStmt(stmt), &new);
                }
                return &t_void;
            },
            .@"if" => |iff| {
                // Unify type of cond with bool
                const c = try self.inferExpr(self.getExpr(iff.cond), env);
                try self.unify(c, &t_bool, &self.sub);
                // Unify body type with else type if exists, otherwise with void
                const b = try self.inferExpr(self.getExpr(iff.body), env);
                const e = if (iff.@"else".get()) |e| try self.inferExpr(self.getExpr(e), env) else &t_void;
                try self.unify(b, e, &self.sub);
                return b;
            },
            .symbol => |symbol| {
                // Iterate each scope and look for symbol
                var e = env;
                while (e.prev != null) : (e = e.prev.?) {
                    if (e.scope.get(symbol)) |t| return self.instantiate(t);
                }
                return error.NameError;
            },
            .int => return &t_int,
            .float => return &t_float,
        }
    }

    /// Check a statement given the current environment.
    pub fn checkStmt(self: *Infer, stmt: ast.Stmt, env: *Env) anyerror!void {
        switch (stmt) {
            .let => |let| {
                var set: TVarSet = .{};
                defer set.deinit(self.allocator);
                try env.freevars(self.allocator, &set);

                const tv = try self.fresh();
                try env.scope.put(self.allocator, let.name, .{ .mono = .{}, .type = tv });
                const t = try self.inferExpr(self.getExpr(let.value), env);
                try self.unify(tv, t, &self.sub);

                try env.scope.put(self.allocator, let.name, try self.generalize(t, &set));
            },
            .brk => std.debug.panic("Not implemented\n", .{}),
            .ret => |ret| {
                //
                if (env.ret) |r| {
                    try self.unify(
                        r,
                        if (ret.get()) |r2| try self.inferExpr(self.getExpr(r2), env) else &t_void,
                        &self.sub,
                    );
                } else {
                    return error.NotInAFunction;
                }
            },
            .expr => |expr| try self.unify(
                try self.inferExpr(self.getExpr(expr), env),
                &t_void,
                &self.sub,
            ),
        }
    }
};
