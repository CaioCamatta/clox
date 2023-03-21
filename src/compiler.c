#include "compiler.h"

#include <stdio.h>

#include "common.h"
#include "scanner.h"

void compile(const char* source) {
    initScanner(source);

    // Temporary code
    // The compiler requests tokens from the scanner as needed. Clox's grammar only requires a single token of lookahead
    int line = -1;
    for (;;) {
        Token token = scanToken();
        if (token.line != line) {
            printf("%4d ", token.line);
            line = token.line;
        } else {
            printf("   | ");
        }
        printf("%2d '%.*s'\n", token.type, token.length, token.start);

        if (token.type == TOKEN_EOF) break;
    }
}