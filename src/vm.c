#include "vm.h"

#include <stdarg.h>
#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"

// TODO (optimization): explicitly make the VM obj a pointer and pass it around.
VM vm;  // Single global VM object; not ideal, but good enough for starters.

static void resetStack() {
    vm.stackTop = vm.stack;
}

/* Print a runtime error. Callers can pass a format string followed by arguments, just like 'printf()'. */
static void runtimeError(const char* format, ...) {
    // Show error message
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    // Show which line of code was being executed when the error occurred.
    // To find the token original token, we look up the line in the debug info compiled into the chunk, which should correspond to the line of source code that the bytecode came from. See emitByte().
    size_t instruction = vm.ip - vm.chunk->code - 1;  // bytecode instruction index
    int line = vm.chunk->lines[instruction];          // look up line using index
    fprintf(stderr, "[line %d] in script\n", line);
    resetStack();
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

// peek(0) peeks the top of the stack
static Value peek(int distance) {
    return vm.stackTop[-1 - distance];
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
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(NUMBER_VAL(-AS_NUMBER(pop())));
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

InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);

    // Send chunk to compiler & populate it with bytecode.
    if (!compile(source, &chunk)) {
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;

    InterpretResult result = run();

    freeChunk(&chunk);
    return result;
}
