#include "scanner.h"

#include <stdio.h>
#include <string.h>

#include "common.h"

typedef struct {
    const char* start;    // start of lexeme
    const char* current;  // current character
    int line;             // for error reporting
} Scanner;

Scanner scanner;  // Global var for simplicity. Ideally this should be passed to function.

void initScanner(const char* source) {
    scanner.start = source;
    scanner.current = source;
    scanner.line = 1;
}

static bool isAtEnd() {
    return *scanner.current == '\0';
}

static Token makeToken(TokenType type) {
    Token token;
    token.type = type;
    token.start = scanner.start;
    token.length = (int)(scanner.current - scanner.start);
    token.line = scanner.line;
    return token;
}

static Token errorToken(const char* message) {
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message; // point to message instead of user's source code
    token.length = (int)strlen(message);
    token.line = scanner.line;
    return token;
}

Token scanToken() {
    scanner.start = scanner.current;

    if (isAtEnd()) return makeToken(TOKEN_EOF);

    return errorToken("Unexpected character.");
}
