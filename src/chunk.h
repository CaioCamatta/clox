#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"

// Each instruction has a one-byte opcode
typedef enum {
    OP_RETURN,
} OpCode;

// Chunk is a dynamic array used to store other data along with instruction
typedef struct {
    int capacity;
    int count;
    uint8_t* code;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte);

#endif