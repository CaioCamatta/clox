#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

typedef struct {
    Chunk* chunk;
    uint8_t* ip;  // Instruction pointer (aka program counter) that pointer to next instruction to be executed
} VM;

void initVM();
void freeVM();
InterpretResult interpret(Chunk* chunk);

#endif