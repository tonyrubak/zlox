const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens.
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,
    // Keywords.
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_ERROR,
    TOKEN_EOF,
};

pub const Token = struct {
    t: TokenType,
    start: []const u8,
    line: usize,
};

pub const Scanner = struct {
    source: []const u8,
    index: usize,
    current: usize,
    line: usize,

    pub fn init(source: []const u8) Scanner {
        return .{
            .source = source,
            .index = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn scanToken(self: *Scanner) Token {
        self.current = self.index;

        if (self.isAtEnd()) return self.makeToken(.TOKEN_EOF);
        const c = self.advance();
        switch (c) {
            '(' => return self.makeToken(.TOKEN_LEFT_PAREN),
            ')' => return self.makeToken(.TOKEN_RIGHT_PAREN),
            '{' => return self.makeToken(.TOKEN_LEFT_BRACE),
            '}' => return self.makeToken(.TOKEN_RIGHT_BRACE),
            ';' => return self.makeToken(.TOKEN_SEMICOLON),
            ',' => return self.makeToken(.TOKEN_COMMA),
            '.' => return self.makeToken(.TOKEN_DOT),
            '-' => return self.makeToken(.TOKEN_MINUS),
            '+' => return self.makeToken(.TOKEN_PLUS),
            '/' => return self.makeToken(.TOKEN_SLASH),
            '*' => return self.makeToken(.TOKEN_STAR),
            '!' => return self.makeToken(if (self.match('=')) .TOKEN_BANG_EQUAL else .TOKEN_BANG),
            '=' => return self.makeToken(if (self.match('=')) .TOKEN_EQUAL_EQUAL else .TOKEN_EQUAL),
            '<' => return self.makeToken(if (self.match('=')) .TOKEN_LESS_EQUAL else .TOKEN_LESS),
            '>' => return self.makeToken(if (self.match('=')) .TOKEN_GREATER_EQUAL else .TOKEN_GREATER),
            else => return self.errorToken("Unexpected character."),
        }
    }

    fn advance(self: *Scanner) u8 {
        const c = self.source[self.index];
        self.index += 1;
        return c;
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.index] != expected) return false;
        self.index += 1;
        return true;
    }

    fn errorToken(self: *const Scanner, message: []const u8) Token {
        const token = Token{
            .t = .TOKEN_ERROR,
            .start = message,
            .line = self.line,
        };

        return token;
    }

    fn makeToken(self: *const Scanner, t: TokenType) Token {
        const token = Token{
            .t = t,
            .start = self.source[self.current..self.index],
            .line = self.line,
        };

        return token;
    }

    fn isAtEnd(self: *const Scanner) bool {
        return self.index >= self.source.len;
    }
};

test "left paren" {
    var scanner = Scanner.init("(");
    try std.testing.expectEqual(scanner.scanToken().t, .TOKEN_LEFT_PAREN);
}

test "equal" {
    var scanner = Scanner.init("=");
    try std.testing.expectEqual(scanner.scanToken().t, .TOKEN_EQUAL);
}

test "equal equal" {
    var scanner = Scanner.init("==");
    try std.testing.expectEqual(scanner.scanToken().t, .TOKEN_EQUAL_EQUAL);
}

test "equal equal then bang" {
    var scanner = Scanner.init("==!");
    try std.testing.expectEqual(scanner.scanToken().t, .TOKEN_EQUAL_EQUAL);
    try std.testing.expectEqual(scanner.scanToken().t, .TOKEN_BANG);
}

test "equal again then dot" {
    var scanner = Scanner.init("=.");
    try std.testing.expectEqual(scanner.scanToken().t, .TOKEN_EQUAL);
    try std.testing.expectEqual(scanner.scanToken().t, .TOKEN_DOT);
}

test "eof" {
    var scanner = Scanner.init("");
    try std.testing.expectEqual(scanner.scanToken().t, .TOKEN_EOF);
}
