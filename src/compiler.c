#include "compiler.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "memory.h"
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
    int depth;        // the scope depth of the block where the local var was declared.
    bool isCaptured;  // is the Local captured by any later nested function declaration?
} Local;

/* Upvalues keep track of closed-over identifiers that the compiler has resolved in the body of each function */
typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

// Differentiates between top-level code and function body
typedef enum {
    TYPE_FUNCTION,
    TYPE_METHOD,
    TYPE_INITIALIZER,
    TYPE_SCRIPT
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];  // locals in scope during compilation. Max 256 in scope at once.
    int localCount;             // number of locals in scope
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;  // number of blocks surrounding current bit of code being compiled
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool hasSuperclass;
} ClassCompiler;

Parser parser;
Compiler* current = NULL;
ClassCompiler* currentClass = NULL;  // Current, innermost class being compiled (used for checking errors when users access 'this')

Chunk* compilingChunk;

static Chunk* currentChunk() {
    return &current->function->chunk;
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

/* Emit loop instruction that jumps backwards to a given offset. */
static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;  // + 2 account for size of OP_LOOP operands
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

/* Emit instruction, write placeholder operand for jump offset (two bytes). Return the offset of the emitted instruction in the chunk. */
static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void emitReturn() {
    // In an initializer, load slop zero which contains the instance
    if (current->type == TYPE_INITIALIZER) {
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);  // Implicitly return nil for functions that don't return anything.
    }

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

/* Go back into the bytecode and replace the operand at given location with the calculated jump offset. */
static void patchJump(int offset) {
    // -2 to adjust for the bytecode for the jump offset itself.
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;
    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(parser.previous.start,
                                             parser.previous.length);
    }

    // Claim the first `locals` slot for the VM's internal use
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";  // Use slot zero to store the instance that 'this' is bound to
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;

#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>");
    }
#endif
    current = current->enclosing;
    return function;
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    // Delete locals
    while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth) {
        // At the end of the block scope, tell the compiler whether the Local needs to be hoisted onto the heap.
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {  // If a variable isn't used by a closure, just pop it!
            // TODO(optimization): Add OP_POPN and use it here
            emitByte(OP_POP);
        }
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
/* Adds an upvalue. An upvalue structure keeps track of closed-over identifiers that the compiler has resolved in the body of each function. */
static int addUpvalue(Compiler* compiler, uint8_t index,
                      bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    // Check if the function already has an upvalue for that variable
    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

/* Look for a local variable declared in a surrounding function. If found, return its upvalue index. If not found, return -1. */
static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    // Go up the Compiler chain looking for an upvalue
    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        // When an upvalue if found, as each recursive call to resolveUpvalue returns, we add an upvalue to the intermediate function.
        // That way, in a program like S { A, x=1 { B { C {print x }}}}, B will have an Upvalue referencing 'x', even though B itself doesn't access x, only C does.
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

/* Initialize next available Local in the compiler's array of local variables. */
static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;  // Sentinel value
    local->isCaptured = false;
}

/* Record the existence of a variable, i.e. add it to the scope */
static void declareVariable() {
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

    addLocal(*name);
}

/* Consume identifier, put it in constant table, return constant index. */
static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    // At runtime, locals aren't looked up by name
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

/* Assign scope depth to variable. */
static void markInitialized() {
    if (current->scopeDepth == 0) return;  // If markInitialized is called by a global...
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

/* Consume arguments and put them on the stack */
static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

/* Compile AND expression. Short-circuit if appropriate. */
static void and_(bool canAssign) {
    // At this point, the left side of the AND has already been compiled. If it's falsey, we short-circuit and skip the right operand.
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
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

/* Function call */
static void call(bool canAssign) {
    uint8_t argCount = argumentList();  // Put N arguments on the stack
    emitBytes(OP_CALL, argCount);
}

/**
 * Dot operator to access instance properties
 * (properties is an any named entity on an instance, fields are properties that have state associated with it.)
 */
static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else if (match(TOKEN_LEFT_PAREN)) {  // This is an optimization that sort of combines OP_GET_PROPERTY and OP_CALL
        uint8_t argCount = argumentList();
        emitBytes(OP_INVOKE, name);
        emitByte(argCount);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
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

static void or_(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
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
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {  // At this point, we know the variable isn't in the current Compiler
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        // Take the given identifier token, add lexeme to chunk's constant table as a string.
        // We need to do this because global vars are referenced by name, and we don't want to put a whole string in the bytecode
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    // if there's an '=' after the identifier, compile the assigned value
    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

/* Load variable using previous consumed token as name. */
static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

/* Create a Token for the given constant string. */
static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

/* Access a superclass method using dot notation */
static void super_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'super' outside of a class.");
    } else if (!currentClass->hasSuperclass) {
        error("Can't use 'super' in a class with no superclass.");
    }

    consume(TOKEN_DOT, "Expect '.' after 'super'.");
    consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
    uint8_t name = identifierConstant(&parser.previous);

    namedVariable(syntheticToken("this"), false);  // Look up current instance in hidden variable "this", push it onto stack

    // Optimization: if we're just about to call the method, combine OP_GET_SUPER and OP_CALL into OP_SUPER_INVOKE
    if (match(TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList();
        namedVariable(syntheticToken("super"), false);  // Look up super class from "super" variable, push onto stack
        emitBytes(OP_SUPER_INVOKE, name);
        emitByte(argCount);
    } else {
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_GET_SUPER, name);  // Encode name of the superclass method to access as operand
    }
}

/* Take previous token (which is 'this') and access it using variable(). */
static void this_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class.");
        return;
    }

    variable(false);  // can't assign to 'this;
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
    [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, dot, PREC_CALL},
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
    [TOKEN_AND] = {NULL, and_, PREC_AND},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, or_, PREC_OR},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {super_, NULL, PREC_NONE},
    [TOKEN_THIS] = {this_, NULL, PREC_NONE},
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

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    // OP_CLOSURE uses variable-size encoding.
    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

static void method() {
    consume(TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = identifierConstant(&parser.previous);

    FunctionType type = TYPE_METHOD;
    // Detect that we're compiling an initializer
    if (parser.previous.length == 4 &&
        memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }

    function(type);
    emitBytes(OP_METHOD, constant);
}

static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser.previous;
    // Add identifier to surrounding function's constant table.
    uint8_t nameConstant = identifierConstant(&parser.previous);
    // Bind name ot a variable
    declareVariable();

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);  // Pops the class

    // Keep track of the fact that we're compiling a class
    ClassCompiler classCompiler;
    classCompiler.hasSuperclass = false;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    // Do the inheritance stuff before declaring methods so the subclass' methods override the superclass'.
    if (match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name.");
        variable(false);  // Load superclass onto stack

        if (identifiersEqual(&className, &parser.previous)) {
            error("A class can't inherit from itself.");
        }

        // To be able to reference the super class, we create a scope and define a variable in it.
        // Then, the methods can automatically access this variable via an upvalue.
        beginScope();
        addLocal(syntheticToken("super"));  // Super is a reserved keyword
        defineVariable(0);

        namedVariable(className, false);  // Load subclass onto stack
        emitByte(OP_INHERIT);             // Wire superclass to new (sub)class
        classCompiler.hasSuperclass = true;
    }

    namedVariable(className, false);  // Insert class name back on the stack to use in methods
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");

    while (!(check(TOKEN_RIGHT_BRACE) || check(TOKEN_EOF))) {
        // Lox doesn't have field declarations.
        // Everything before the end of the body must be a method
        method();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(OP_POP);  // Pop class

    if (classCompiler.hasSuperclass) {
        endScope();
    }

    currentClass = currentClass->enclosing;
}

static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name.");
    markInitialized();  // mark function as initialized so we can reference it inside the body as we compile it. This enables recursion.
    function(TYPE_FUNCTION);
    defineVariable(global);
}

static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

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
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    // ... so we pop the value off the stack and forget it.
    emitByte(OP_POP);
}

static void forStatement() {
    beginScope();

    // Initializer
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(TOKEN_SEMICOLON)) {
        // No initializer.
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;

    // Condition clause
    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
    }

    // Increment clause - jump over increment, run body, jump back to increment, run increment, go to next iteration
    if (!match(TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(loopStart);         // Go back to top of the for loop
        loopStart = incrementStart;  // future interations should jump back to increment
        patchJump(bodyJump);
    }

    statement();
    emitLoop(loopStart);

    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP);  // Condition.
    }

    endScope();
}

static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);  // Pop condition from the stack
    statement();

    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    emitByte(OP_POP);

    if (match(TOKEN_ELSE)) statement();
    patchJump(elseJump);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer.");
        }

        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

static void whileStatement() {
    int loopStart = currentChunk()->count;  // At this point we already know where to jump back to.
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP);
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
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    // If we hit a compile error whiler parsing the previous statement, enter panic.
    if (parser.panicMode) synchronize();
}

static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

/* Read char* source and write the code to chunk. */
ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}

/* Mark reachable objects for GC. In practice these are just ObjFunctions we're compiling into. */
void markCompilerRoots() {
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}