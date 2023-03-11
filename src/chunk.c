#include "chunk.h"

#include <limits.h>
#include <stdlib.h>

#include "memory.h"

/* Initialize empty dynamic array */
void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    initValueArray(&chunk->constants);
}

/* Write to dynamic array. This function will update the size of the array if needed. */
void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    // Keep line info in array parallel with code
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

/* Add value to chunks constant arary and write an appropriate instruction to load the constant.*/
void writeConstant(Chunk* chunk, Value value, int line) {
    OpCode instruction;
    int constant_index_size;

    if (chunk->constants.count < UCHAR_MAX) {
        instruction = OP_CONSTANT;
        constant_index_size = 1;
    } else {
        instruction = OP_CONSTANT_LONG;
        constant_index_size = 3;
    }

    // If needed, grow array by (instruction byte + constant index)
    if (chunk->capacity < chunk->count + (1 + constant_index_size)) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
    }

    // Write value to array
    int constant_index = addConstant(chunk, value);

    // Write instruction
    chunk->code[chunk->count] = instruction;
    // Keep line info in array parallel with code
    chunk->lines[chunk->count] = line;

    // Then write constant index in the next 1 or 3 blocks
    if (constant_index < UCHAR_MAX) {
        chunk->code[chunk->count + 1] = constant_index;
        chunk->lines[chunk->count + 1] = line;
    } else {
        chunk->code[chunk->count + 1] = (uint8_t)((constant_index & 0x00FF0000) >> 16);
        chunk->code[chunk->count + 2] = (uint8_t)((constant_index & 0x0000FF00) >> 8);
        chunk->code[chunk->count + 3] = (uint8_t)(constant_index & 0x000000FF);
        chunk->lines[chunk->count + 1] = line;
        chunk->lines[chunk->count + 2] = line;
        chunk->lines[chunk->count + 3] = line;
    }

    chunk->count = chunk->count + constant_index_size + 1;
}

/*  Add a constant to the chunk, return its index */
int addConstant(Chunk* chunk, Value value) {
    writeValueArray(&chunk->constants, value);
    return chunk->constants.count - 1;
}

/* Delete array and free its memory */
void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    freeValueArray(&chunk->constants);
    initChunk(chunk);  // Zero out the fields to clean up the chunk
}