#include "chunk.h"
#include "common.h"
#include "debug.h"

int main(int arc, const char *argv[]) {
    Chunk chunk;
    initChunk(&chunk);

    for (int i = 0; i < 300; i++) {
        writeConstant(&chunk, 1000 + i, 10);
    }

    writeChunk(&chunk, OP_RETURN, 42);

    disassembleChunk(&chunk, "test chunk");
    freeChunk(&chunk);
    return 0;
}