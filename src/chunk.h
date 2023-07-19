#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

// Each instruction has a one-byte opcode
typedef enum {
    OP_CONSTANT,  // Two bytes: opcode, constant index (operand)
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_INVOKE,
    OP_CLOSURE,
    OP_CLOSE_UPVALUE,
    OP_RETURN,  // One-byte opcode
    OP_CLASS,
    OP_INHERIT,
    OP_METHOD,
} OpCode;

// A "Chunk" of code.
// Chunk is a dynamic array used to store other data along with instruction
typedef struct {
    int capacity;
    int count;
    uint8_t* code;
    int* lines;  // Keeps track of line numbers for the bytecode
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif