#include "vm.h"

#include <stdio.h>

#include "common.h"
#include "debug.h"

// TODO (optimization): explicitly make the VM obj a pointer and pass it around.
VM vm;  // Single global VM object; not ideal, but good enough for starters.

void initVM() {
}

void freeVM() {
}

/* The heart of the VM. Most of the VM's time is spent in this function. */
static InterpretResult run() {
// Read the byte IP points at and advance IP.
#define READ_BYTE() (*vm.ip++)
// Read next byte from bytecode, treat resulting number as an index, use the index to lookup the corresponding Value in the chunks constant table
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        // Pointer math; code is stored contiguously
        disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif

        uint8_t instruction;
        // Decode/dispatch the instruction
        // TODO (optimization): research different bytecode dispatching techniques (not simple)
        switch (instruction = READ_BYTE()) {  // The first byte of any instruction is the opcode.
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                printValue(constant);
                printf("\n");
                break;
            }
            case OP_RETURN: {
                return INTERPRET_OK;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
}

InterpretResult interpret(Chunk* chunk) {
    vm.chunk = chunk;
    // TODO (optimization): store ip in local var so we keep it in a register. Reason: ip gets modified *very* often.
    vm.ip = vm.chunk->code;  // initialize IP by pointing to first instruction in chunk of code.
    return run();
}