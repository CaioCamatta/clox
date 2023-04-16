#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_STRING(value) isObjType(value, OBJ_STRING)

// These macros take a Value that is expected to have a pointer to a valid ObjString
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_STRING,
} ObjType;

struct Obj {
    ObjType type;
    struct Obj* next;  // Intrusive pointer to next object for GC
};

// ObjString can be cast to Obj, which allows for a form of OOP.
// This is because C struct fields are arranged in memory in the order that they are declared
struct ObjString {
    Obj obj;     // state all Objs share
    int length;  // length without \0
    char* chars;
    uint32_t hash;
};

ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);

// We use a function outside of the macro to provent expanding `value` twice
static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif