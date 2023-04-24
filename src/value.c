#include "value.h"

#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void writeValueArray(ValueArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values,
                                   oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

void printValue(Value value) {
    switch (value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NIL:
            printf("nil");
            break;
        case VAL_NUMBER:
            printf("%g", AS_NUMBER(value));
            break;
        case VAL_OBJ:
            printObject(value);
            break;
        case VAL_EMPTY:
            printf("empty");
    }
}

bool valuesEqual(Value a, Value b) {
    if (a.type != b.type) return false;
    switch (a.type) {
        case VAL_BOOL:
            return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL:
            return true;
        case VAL_NUMBER:
            return AS_NUMBER(a) == AS_NUMBER(b);
        case VAL_OBJ:
            return AS_OBJ(a) == AS_OBJ(b);
        default:
            return false;  // Unreachable.
    }
}

/* Generate hash for a Double.
Uses the same algorithm as Lua, which maps the mantissa to the integer range and combines it with the exponent.
Reference: https://readafterwrite.wordpress.com/2017/03/23/floating-point-keys-in-lua/
https://github.com/lua/lua/blob/e15f1f2bb7a38a3c94519294d031e48508d65006/ltable.c#L131
*/
static uint32_t hashDouble(double n) {
    if (isinf(n) || isnan(n))
        return 0;

    // Signed mantissa
    // n = m·2^exp. mantissa is normalized so |m| in [0.5,1)
    int32_t exp;
    double m = frexp(n, &exp);

    // Map mantissa to integer range
    // There's a bit of magic here, but essentially it extracts (only) the most significant bits
    int32_t mi = (int32_t)(m * -(double)INT_MIN);

    // Add mantissa and exponent
    uint32_t u = (uint32_t)exp + (uint32_t)mi;
    return u >= (uint32_t)INT_MAX ? u : ~u;
}

u_int32_t hashValue(Value value) {
    switch (value.type) {
        case VAL_BOOL:
            return AS_BOOL(value) ? 2 : 1;
        case VAL_NIL:
            return 3;
        case VAL_NUMBER:
            return hashDouble(AS_NUMBER(value));
        case VAL_OBJ:
            return AS_STRING(value)->hash;
        case VAL_EMPTY:
            return 0;
        default:
            break;
    }
}
