#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

// Each instruction has a one-byte opcode
typedef enum {
    OP_RETURN, // One-byte opcode
    OP_CONSTANT, // Two bytes: opcode, constant index (operand)
    OP_CONSTANT_LONG, // Four bytes: opcode, 24-bit constant index (operand)
} OpCode;

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
void writeConstant(Chunk* chunk, Value value, int line);
int addConstant(Chunk* chunk, Value value);

#endif