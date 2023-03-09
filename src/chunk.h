#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

// Each instruction has a one-byte opcode
typedef enum {
    OP_RETURN,
    OP_CONSTANT,
} OpCode;

// Run-length encoded line information
typedef struct {
    int capacity;
    int *lines;
    int *count;
} LineInfo;

// Chunk is a dynamic array used to store other data along with instruction
typedef struct {
    int capacity;
    int count;
    uint8_t* code;
    LineInfo lineInfo;  // Keeps track of line numbers for the bytecode
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif