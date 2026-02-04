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
    lexeme: []const u8,
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
        self.skipWhitespace();
        self.current = self.index;

        if (self.isAtEnd()) return self.makeToken(.TOKEN_EOF);
        const c = self.advance();

        if (std.ascii.isDigit(c)) return self.number();
        if (isAlpha(c)) return self.identifier();

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
            '"' => return self.string(),
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
            .lexeme = message,
            .line = self.line,
        };

        return token;
    }

    fn skipWhitespace(self: *Scanner) void {
        while (self.peek()) |c| {
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    const slash_next = if (self.peekNext()) |next| next == '/' else false;
                    if (!slash_next) return;
                    while (self.peek() != null and self.peek().? != '\n') _ = self.advance();
                },
                else => return,
            }
        }
    }

    fn peek(self: *Scanner) ?u8 {
        if (self.isAtEnd()) return null;
        return self.source[self.index];
    }

    fn peekNext(self: *Scanner) ?u8 {
        if (self.isAtEnd()) return null;
        return self.source[self.index + 1];
    }

    fn identifier(self: *Scanner) Token {
        while (self.peek()) |c| {
            if (isAlpha(c) or std.ascii.isDigit(c)) {
                _ = self.advance();
            } else {
                break;
            }
        }
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Scanner) TokenType {
        const map = std.StaticStringMap(TokenType).initComptime(.{
            .{ "and", .TOKEN_AND },
            .{ "class", .TOKEN_CLASS },
            .{ "else", .TOKEN_ELSE },
            .{ "false", .TOKEN_FALSE },
            .{ "for", .TOKEN_FOR },
            .{ "fun", .TOKEN_FUN },
            .{ "if", .TOKEN_IF },
            .{ "nil", .TOKEN_NIL },
            .{ "or", .TOKEN_OR },
            .{ "print", .TOKEN_PRINT },
            .{ "return", .TOKEN_RETURN },
            .{ "super", .TOKEN_SUPER },
            .{ "this", .TOKEN_THIS },
            .{ "true", .TOKEN_TRUE },
            .{ "var", .TOKEN_VAR },
            .{ "while", .TOKEN_WHILE },
        });

        return map.get(self.source[self.current..self.index]) orelse .TOKEN_IDENTIFIER;
    }

    fn string(self: *Scanner) Token {
        while (self.peek()) |c| {
            if (c == '"') break;

            if (c == '\n') self.line += 1;

            _ = self.advance();
        }

        if (self.isAtEnd()) return self.errorToken("Unterminated string.");

        _ = self.advance();
        return self.makeToken(.TOKEN_STRING);
    }

    fn number_helper(self: *Scanner) void {
        while (self.peek()) |c| {
            if (std.ascii.isDigit(c)) {
                _ = self.advance();
            } else {
                break;
            }
        }
    }

    fn number(self: *Scanner) Token {
        self.number_helper();

        const c = self.peek() orelse return self.makeToken(.TOKEN_NUMBER);
        const next = self.peekNext() orelse return self.makeToken(.TOKEN_NUMBER);

        if (c == '.' and std.ascii.isDigit(next)) {
            _ = self.advance();
            self.number_helper();
        }

        return self.makeToken(.TOKEN_NUMBER);
    }

    fn makeToken(self: *const Scanner, t: TokenType) Token {
        const token = Token{
            .t = t,
            .lexeme = self.source[self.current..self.index],
            .line = self.line,
        };

        return token;
    }

    fn isAtEnd(self: *const Scanner) bool {
        return self.index >= self.source.len;
    }
};

fn isAlpha(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

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

test "eats whitespace then equal" {
    var scanner = Scanner.init("  =");
    try std.testing.expectEqual(.TOKEN_EQUAL, scanner.scanToken().t);
}

test "eats whitespace then equal then whitespace then bang" {
    var scanner = Scanner.init("  =\t!");
    try std.testing.expectEqual(.TOKEN_EQUAL, scanner.scanToken().t);
    try std.testing.expectEqual(.TOKEN_BANG, scanner.scanToken().t);
}

test "comment then bang" {
    var scanner = Scanner.init("// this is a comment\n!");
    try std.testing.expectEqual(.TOKEN_BANG, scanner.scanToken().t);
}

test "equal then comment" {
    var scanner = Scanner.init("=\n//a comment");
    try std.testing.expectEqual(.TOKEN_EQUAL, scanner.scanToken().t);
    try std.testing.expectEqual(.TOKEN_EOF, scanner.scanToken().t);
}

test "a string" {
    var scanner = Scanner.init("\"a string\"");
    const token = scanner.scanToken();
    try std.testing.expectEqual(.TOKEN_STRING, token.t);
    try std.testing.expectEqualStrings("\"a string\"", token.lexeme);
}

test "oh no my string" {
    var scanner = Scanner.init("\"oh no my string");
    const token = scanner.scanToken();
    try std.testing.expectEqual(.TOKEN_ERROR, token.t);
}

test "a number" {
    var scanner = Scanner.init("10");
    const token = scanner.scanToken();
    try std.testing.expectEqual(.TOKEN_NUMBER, token.t);
    try std.testing.expectEqualStrings("10", token.lexeme);
}

test "a dotted number" {
    var scanner = Scanner.init("3.14");
    const token = scanner.scanToken();
    try std.testing.expectEqual(.TOKEN_NUMBER, token.t);
    try std.testing.expectEqualStrings("3.14", token.lexeme);
}

test "an identifier" {
    var scanner = Scanner.init("foo");
    const token = scanner.scanToken();
    try std.testing.expectEqual(.TOKEN_IDENTIFIER, token.t);
}

test "a keyword???" {
    var scanner = Scanner.init("and");
    const token = scanner.scanToken();
    try std.testing.expectEqual(.TOKEN_AND, token.t);
}
