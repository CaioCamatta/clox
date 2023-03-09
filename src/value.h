#ifndef clox_value_h
#define clox_value_h

#include "common.h"

// Values are doubles that OP_CONSTANTS refer to (via an index).
// This is an alternative to Immediate Instructions (i.e. including the value immediately in front of the operator)
typedef double Value;

// Value array is similar to the JVM's constant pool
typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif