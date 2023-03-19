#include "vm.h"

#include <stdio.h>

#include "common.h"
#include "debug.h"

// TODO (optimization): explicitly make the VM obj a pointer and pass it around.
VM vm;  // Single global VM object; not ideal, but good enough for starters.

static void resetStack() {
    vm.stackTop = vm.stack;
}

void initVM() {
    resetStack();
}

void freeVM() {
}

void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

/* The heart of the VM. Most of the VM's time is spent in this function. */
static InterpretResult run() {
// Read the byte IP points at and advance IP.
#define READ_BYTE() (*vm.ip++)

// Read next byte from bytecode, treat resulting number as an index, use the index to lookup the corresponding Value in the chunks constant table
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

#define BINARY_OP(op)     \
    do {                  \
        double b = pop(); \
        double a = pop(); \
        push(a op b);     \
    } while (false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        // Pointer math; code is stored contiguously
        disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif

        uint8_t instruction;
        // Decode/dispatch the instruction
        // TODO (optimization): research different bytecode dispatching techniques (not simple)
        switch (instruction = READ_BYTE()) {  // The first byte of any instruction is the opcode.
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_ADD:
                BINARY_OP(+);
                break;
            case OP_SUBTRACT:
                BINARY_OP(-);
                break;
            case OP_MULTIPLY:
                BINARY_OP(*);
                break;
            case OP_DIVIDE:
                BINARY_OP(/);
                break;
            case OP_NEGATE:
                push(-pop());
                break;
            case OP_RETURN: {
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(Chunk* chunk) {
    vm.chunk = chunk;
    // TODO (optimization): store ip in local var so we keep it in a register. Reason: ip gets modified *very* often.
    vm.ip = vm.chunk->code;  // initialize IP by pointing to first instruction in chunk of code.
    return run();
}