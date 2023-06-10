#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    ObjFunction* function;
    uint8_t* ip;   // Instruction pointer (aka program counter) that pointer to next instruction to be executed
    Value* slots;  // where the function's locals begin
} CallFrame;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frameCount;  // height of the CallFrame stack == number of ongoing function calls

    Value stack[STACK_MAX];  // Array is declared directly inline
    Value* stackTop;         // Use actual pointer instead of int index (faster dereferencing)
    Table globals;           // Global vars
    Table strings;           // Stores ("interns") every string that's been created. Used for string deduplication.
    Obj* objects;            // Pointer to head of objects linked list
} VM;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif