#ifndef clox_value_h
#define clox_value_h

#include "common.h"

// The VM's notion of type
typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
} ValueType;

// Values are doubles that OP_CONSTANTS refer to (via an index).
// (This is an alternative to Immediate Instructions (i.e. including the value immediately in front of the operator))
typedef struct {
    ValueType type;  // Tag determines if union uses boolean or number
    union {          // "tagged union" which different "cases"
        bool boolean;
        double number;
    } as;  // name of the union
} Value;

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)

// Unpack a value and get the C value back out.
// 'value' needs to be of the correct type! Use the IS_ macros to check.
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
// no AS_NIL since there's only one `nil` value.

// Convert a native C value to a clox Value
#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})

// Value array is similar to the JVM's constant pool
typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif