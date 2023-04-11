#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

typedef struct {
    Chunk* chunk;
    uint8_t* ip;             // Instruction pointer (aka program counter) that pointer to next instruction to be executed
    Value stack[STACK_MAX];  // Array is declared directly inline
    Value* stackTop;         // Use actual pointer instead of int index (faster dereferencing)
    Obj* objects;            // Pointer to head of objects linked list
} VM;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif