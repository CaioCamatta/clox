#include "compiler.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;  // Used for error handling to avoid error cascades.
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

// ParseFn is a function type that takes no args and returns nothing.
typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

// Local variable
typedef struct {
    Token name;
    int depth;  // the scope depth of the block where the local var was declared.
    bool isConst;
} Local;

typedef struct {
    Local locals[UINT8_COUNT];         // locals in scope during compilation. Max 256 in scope at once.
    uint8_t constLocals[UINT8_COUNT];  // constant locals in scope during compilation. Max 256 in scope at once.
    Table constGlobals;                // constant globals.
    int localCount;                    // number of locals in scope
    int scopeDepth;                    // number of blocks surrounding current bit of code being compiled
} Compiler;

Parser parser;
Compiler* current = NULL;
Chunk* compilingChunk;

static Chunk* currentChunk() {
    return compilingChunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;

    // Enter panic mode when we find an error
    parser.panicMode = true;

    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

// Error at the location of the token just consumed
static void error(const char* message) {
    errorAt(&parser.previous, message);
}

// Errors at token to-be-consumed
static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

// Scan tokens & advance parser while current token is not a TOKEN_ERROR
static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        // In clox, the parser reports errors, not the scanner
        errorAtCurrent(parser.current.start);
    }
}

// Consume provided type & advance, or error
static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

/* Check if current token is of a given type. */
static bool check(TokenType type) {
    return parser.current.type == type;
}

/* Consume current token if it's of a given type. Returns true if a token was consumed and false otherwise. */
static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

/* Write given byte (opcode or operand) and record line information. */
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

/* Emit two bytes (convinience function meant for opcode + operand) */
static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitReturn() {
    emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    // OP_CONSTANT uses a single byte for the index operand
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void initCompiler(Compiler* compiler) {
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    memset(compiler->constLocals, 0, sizeof(compiler->constLocals));
    initTable(&compiler->constGlobals);
    current = compiler;
}

static void endCompiler() {
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    // Delete locals
    while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth) {
        // TODO(optimization): Add OP_POPN and use it here
        emitByte(OP_POP);
        current->localCount--;
    }
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precendence);

/* Add token's lexeme to the Chunk's constant table. */
static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start,
                                           name->length)));
}

/* Check if two identifiers are the same */
static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

/* Resolve local variable. Returns the variable's index in the Compiler's array of locals or -1 if not found.

The indices int the Compiler's locals array matches the VM's stack at runtime.  */
static int resolveLocal(Compiler* compiler, Token* name) {
    // Walk list of locals currently in scope
    // Walk back so inner locals shadow outer ones
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }
    return -1;  // Assume to be a local
}

/* Initialize next available Local in the compiler's array of local variables. */
static void addLocal(Token name, bool isConst) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;  // Sentinel value
    local->isConst = isConst;
}

/* Record the existence of a variable, i.e. add it to the scope */
static void declareVariable(bool isConst) {
    // Not applicable to globals
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;

    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name, isConst);
}

/* Consume identifier, put it in constant table, return constant index. */
static uint8_t parseVariable(const char* errorMessage, bool isConst) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable(isConst);
    // At runtime, locals aren't looked up by name
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

/* Assign scope depth to variable, optionally mark it as final/constant. */
static void markInitialized() {
    current->locals[current->localCount - 1].depth = current->scopeDepth;
};

/* Output bytecode instruction that defines the new variable and stores its initial value. Make variable available for use.

The instruction’s operand is the index of the variable’s name in the Chunk's constant table. */
static void defineVariable(uint8_t global) {
    // Locals aren'tstored in the constants table
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

/* Binary operator. Left associative.
How it works: Push left and right onto stack, then operation. The VM will execute left, right, then pop both, compute, and push result. */
static void binary(bool canAssign) {
    // Operator has already been consumed here
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    // Right operand
    // Example: "2 * 3 + 4", here, the right operand is just 3, because + is lower precedence than *. So parse only one level higher than operator.
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_BANG_EQUAL:
            // A '!=' is the same as an '==' followed by a '!'
            // a != b has the same semantics as !(a == b)
            emitBytes(OP_EQUAL, OP_NOT);
            break;
        case TOKEN_EQUAL_EQUAL:
            emitByte(OP_EQUAL);
            break;
        case TOKEN_GREATER:
            emitByte(OP_GREATER);
            break;
        case TOKEN_GREATER_EQUAL:
            emitBytes(OP_LESS, OP_NOT);
            break;
        case TOKEN_LESS:
            emitByte(OP_LESS);
            break;
        case TOKEN_LESS_EQUAL:
            emitBytes(OP_GREATER, OP_NOT);
            break;
        case TOKEN_PLUS:
            emitByte(OP_ADD);
            break;
        case TOKEN_MINUS:
            emitByte(OP_SUBTRACT);
            break;
        case TOKEN_STAR:
            emitByte(OP_MULTIPLY);
            break;
        case TOKEN_SLASH:
            emitByte(OP_DIVIDE);
            break;
        default:
            return;  // Unreachable.
    }
}

/* Push literals onto the stack */
static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE:
            emitByte(OP_FALSE);
            break;
        case TOKEN_NIL:
            emitByte(OP_NIL);
            break;
        case TOKEN_TRUE:
            emitByte(OP_TRUE);
            break;
        default:
            return;  // Impossible.
    }
}

/* Prefix expression that starts with '(' */
static void grouping(bool canAssign) {
    // Assume initial '(' has already been consumed
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
    // Assume the token for the number literal has already been consumed
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string(bool canAssign) {
    // trim quotes
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                                    parser.previous.length - 2)));
}

/* Emit instruciton to load variable. */
static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else {
        // Take the given identifier token, add lexeme to chunk's constant table as a string.
        // We need to do this because global vars are referenced by name, and we don't want to put a whole string in the bytecode
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    ObjString* nameStr = AS_STRING(OBJ_VAL(copyString(name.start,
                                                      name.length)));
    Value value;
    bool isGlobalConstant = tableGet(&current->constGlobals, nameStr, &value);

    // if there's an '=' after the identifier, compile the assigned value
    if (canAssign && match(TOKEN_EQUAL)) {
        if ((setOp == OP_SET_LOCAL && current->locals[arg].isConst) || (setOp == OP_SET_GLOBAL && isGlobalConstant)) {
            error("Cannot reassign val.");
        }
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile operand
    parsePrecedence(PREC_UNARY);  // Only parse unary or higher. Prevents cases such as "-a.b + c" where we wouldn't want all of "a.b + c" to be negated

    // Emit the operator instruction
    switch (operatorType) {
        case TOKEN_BANG:
            emitByte(OP_NOT);
            break;
        case TOKEN_MINUS:
            // Emit negation *after* operand, so we can pop operand, negate it, and push it back to the stack
            emitByte(OP_NEGATE);
            break;
        default:
            return;  // Impossible.
    }
}

/* Given a token type, find
 *  1. the function to compile a prefix expressions starting w/ that type
 *  2. the function to compile an infix expression whole left operand is followed by a token of that type
 *  3. the precedence of an infix expression that uses that token as operator
 *
 * (We don't care about the precedence of the prefix expression starting with a given token because all prefix operators in Lox have the same precendece)
 *
 * Initializer syntax for rules: [TOKEN_TYPE is a number from the enumeration]
 *
 * TODO(optimization): add more specific instrucitons and benchmark (e.g. small int constants, adding and subtracting 1, etc*/
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, NULL, PREC_NONE},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, NULL, PREC_NONE},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, NULL, PREC_NONE},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, NULL, PREC_NONE},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

/* Start a current level and parse any expression at the given precedence level or higher. */
static void parsePrecedence(Precedence precedence) {
    advance();

    // The first token is always going to belong to some prefix expression, by definition.
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    // If there's no prefix parse, it must be a syntax error.
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    // Only low precendence expressions can assign.
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    // Effectively, the only time we allow an assignment is when parsing an assignment expression or top level expression.
    prefixRule(canAssign);

    // Look for an infix parser as next token. If there is one, then the prefix expression already compiled might be an operand for it.
    // If current token's precendece is lower, dont consume it.
    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    // Prevents errors for invalid expressions, e.g., 'a * b = c + d;'
    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

/* Return rule at the given index */
static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

/* Declare reassignable variable */
static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.", false);

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        // Basically desugar 'var a;' into 'var a = nil;'
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON,
            "Expect ';' after variable declaration.");

    defineVariable(global);
}

/* Declare single-assigment/constant/final variable */
static void valDeclaration() {
    uint8_t global = parseVariable("Expect variable name.", true);

    // Mark global as constant
    if (current->scopeDepth == 0) {
        ObjString* name = copyString(parser.previous.start, parser.previous.length);
        Value value = BOOL_VAL(true);
        if (!tableSet(&current->constGlobals, name, value)) {
            error("Error creating global.");
        }
    }

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        // Basically desugar 'var a;' into 'var a = nil;'
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON,
            "Expect ';' after variable declaration.");

    defineVariable(global);
}

/* An expression statement is simple an expression where a statement is expected.
It's an expression followed by a semicolon, such as a function call, e.g., > eat(brunch); */
static void expressionStatement() {
    // Expressions have a stack effect of +1...
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression");
    // ... so we pop the value off the stack and forget it.
    emitByte(OP_POP);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void synchronize() {
    parser.panicMode = false;

    // Skip tokens until we find a statement boundary or a semicolon.
    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;

            default:;  // Do nothing.
        }
        advance();
    }
}

/* Declarations is one of the two types of statements supported by Lox. They bind a new name to a value. */
static void declaration() {
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else if (match(TOKEN_VAL)) {
        valDeclaration();
    } else {
        statement();
    }

    // If we hit a compile error whiler parsing the previous statement, enter panic.
    if (parser.panicMode) synchronize();
}

static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

/* Read char* source and write the code to chunk. */
bool compile(const char* source, Chunk* chunk) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    endCompiler();
    return !parser.hadError;
}