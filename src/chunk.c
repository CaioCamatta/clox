#include "chunk.h"

#include <stdlib.h>

#include "memory.h"
#include "vm.h"

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
    // TODO (optimization) use run-length encoding to reduce size of lines array.
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

/*  Add a constant to the chunk, return its index */
int addConstant(Chunk* chunk, Value value) {
    push(value);  // Protect from GC
    writeValueArray(&chunk->constants, value);
    pop();
    return chunk->constants.count - 1;
}

/* Delete array and free its memory */
void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    freeValueArray(&chunk->constants);
    initChunk(chunk);  // Zero out the fields to clean up the chunk
}