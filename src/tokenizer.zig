const std = @import("std");

pub fn tokenize(allocator: std.mem.Allocator, src: []const u8) !std.MultiArrayList(Token) {
    var toks: std.MultiArrayList(Token) = .{};
    var t = Tokenizer{ .src = src };
    while (t.next()) |tok| {
        try toks.append(allocator, tok);
    }
    return toks;
}

const Tokenizer = struct {
    src: []const u8,
    i: u32 = 0,
    tok_start: u32 = undefined,

    fn next(self: *Tokenizer) ?Token {
        if (self.i == self.src.len) {
            return null;
        }
        switch (self.src[self.i]) {
            'a'...'z', 'A'...'Z', '_' => return self.sym(),
            '0'...'9' => return self.num(),
            '"', '\'' => return self.str(),

            ' ', '\t', '\n' => {
                self.start();
                while (self.i < self.src.len) : (self.i += 1) {
                    switch (self.src[self.i]) {
                        ' ', '\t', '\n' => {},
                        else => break,
                    }
                }
                return self.tok(.ws);
            },

            else => {
                self.start();
                return self.tok(.invalid);
            },
        }
    }

    fn sym(self: *Tokenizer) Token {
        self.start();
        while (self.i < self.src.len) : (self.i += 1) {
            switch (self.src[self.i]) {
                '0'...'9', 'a'...'z', 'A'...'Z', '_' => {},
                else => break,
            }
        }

        return self.tok(
            keywords.get(self.currentTok()) orelse .symbol,
        );
    }

    fn num(self: *Tokenizer) Token {
        self.start();
        var float = false;
        while (self.i < self.src.len) : (self.i += 1) {
            switch (self.src[self.i]) {
                '0'...'9' => {},
                '.' => if (float) {
                    break;
                } else {
                    float = true;
                },
                else => break,
            }
        }
        return self.tok(if (float) .float else .integer);
    }

    fn str(self: *Tokenizer) Token {
        self.start();
        const delim = self.src[self.i];
        var esc = true;
        while (self.i < self.src.len) : (self.i += 1) {
            const c = self.src[self.i];
            if (esc) {
                esc = false;
            } else if (c == '\\') {
                esc = true;
            } else if (c == delim) {
                self.i += 1;
                break;
            }
        } else {
            return self.tok(.invalid);
        }

        return self.tok(switch (delim) {
            '"' => .string,
            '\'' => .rune,
            else => unreachable,
        });
    }

    fn start(self: *Tokenizer) void {
        self.tok_start = self.i;
    }
    fn tok(self: *Tokenizer, ty: Token.Type) Token {
        // TODO: handle token >65535
        const len = std.math.lossyCast(u16, self.i - self.tok_start);
        self.tok_start = undefined;
        return Token{
            .type = ty,
            .len = len,
        };
    }
    fn currentTok(self: *Tokenizer) []const u8 {
        return self.src[self.tok_start..self.i];
    }
};

pub const Token = struct {
    type: Type,
    len: u16,

    pub const Type = enum {
        invalid,
        ws,
        comment,

        symbol,
        integer,
        float,
        rune,
        string,

        kw_break,
        kw_else,
        kw_fn,
        kw_if,
        kw_let,
        kw_return,
    };
};

const keywords = std.ComptimeStringMap(Token.Type, .{
    .{ "break", .kw_break },
    .{ "else", .kw_else },
    .{ "fn", .kw_fn },
    .{ "if", .kw_if },
    .{ "let", .kw_let },
    .{ "return", .kw_return },
});

test "tokenizer" {
    try testTok(&.{
        .symbol,   .ws, .symbol,    .ws,
        .integer,  .ws, .float,     .ws,
        .string,   .ws, .string,    .ws,
        .rune,     .ws, .rune,      .ws,

        .kw_if,    .ws, .kw_else,   .ws,
        .kw_let,   .ws, .kw_fn,     .ws,
        .kw_break, .ws, .kw_return, .ws,
    },
        \\abcd _f00bar
        \\100 3.14
        \\"Hello, world!" "\"Hello, world!\" said Bob"
        \\'\'' 'x'
        \\if else
        \\let fn
        \\break return
        \\
    );
}
fn testTok(
    expected: []const Token.Type,
    src: []const u8,
) !void {
    var toks = try tokenize(std.testing.allocator, src);
    defer toks.deinit(std.testing.allocator);

    try std.testing.expectEqual(expected.len, toks.len);

    var pos: u32 = 0;
    const s = toks.slice();
    for (s.items(.type)) |tok, i| {
        try std.testing.expectEqual(expected[i], tok);
        pos += s.items(.len)[i];
    }

    try std.testing.expectEqual(src.len, pos);
}
