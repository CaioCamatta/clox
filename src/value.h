#ifndef clox_value_h
#define clox_value_h

#include <string.h>

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOXING

// non number values use the quiet NaN flag (bit 51 in IEEE-754) + all exponent bits + bit 50 (intel FP Indef. edge case)
#define QNAN ((uint64_t)0x7ffc000000000000)
#define SIGN_BIT ((uint64_t)0x8000000000000000)

// Lowest bits of the unused mantissa
#define TAG_NIL 1    // 01.
#define TAG_FALSE 2  // 10.
#define TAG_TRUE 3   // 11.

typedef uint64_t Value;

#define IS_BOOL(value) (((value) | 1) == TRUE_VAL)  // Makes it so FALSE_VAL become TRUE_VAL and we only need one comparison.
#define IS_NIL(value) ((value) == NIL_VAL)
#define IS_NUMBER(value) (((value)&QNAN) != QNAN)
#define IS_OBJ(value) \
    (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(value) ((value) == TRUE_VAL)  // In lox, only "true" is truthy
#define AS_NUMBER(value) valueToNum(value)
#define AS_OBJ(value) \
    ((Obj*)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))  // tilde will clear the signbit and QNAN

#define BOOL_VAL(b) ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define NIL_VAL ((Value)(uint64_t)(QNAN | TAG_NIL))
#define NUMBER_VAL(num) numToValue(num)
// In practice, the system will only use the lower 48 bits for memory pointers, so this is safe:
#define OBJ_VAL(obj) \
    (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))  // The sign bit differentiates between object and number.

// Convert double to Value.
static inline Value numToValue(double num) {
    Value value;
    // This is the supported way for type punning so the compiler should optimize away the memcpy.
    memcpy(&value, &num, sizeof(double));
    return value;
}

// Convert double to Value
static inline double valueToNum(Value value) {
    double num;
    memcpy(&num, &value, sizeof(Value));
    return num;
}

#else

// The VM's notion of type
typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ,  // any value whose state lives on the heap
} ValueType;

// Values are doubles that OP_CONSTANTS refer to (via an index).
// (This is an alternative to Immediate Instructions (i.e. including the value immediately in front of the operator))
typedef struct {
    ValueType type;  // Tag determines if union uses boolean or number
    union {          // "tagged union" which different "cases"
        bool boolean;
        double number;
        Obj* obj;
    } as;  // name of the union
} Value;

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)

// Unpack a value and get the C value back out.
// 'value' needs to be of the correct type! Use the IS_ macros to check.
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value) ((value).as.obj)
// no AS_NIL since there's only one `nil` value.

// Convert a native C value to a clox Value
#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object) ((Value){VAL_OBJ, {.obj = (Obj*)object}})

#endif

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