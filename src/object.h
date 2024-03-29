#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "table.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value) isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_BOUND_METHOD(value) ((ObjBoundMethod*)AS_OBJ(value))
#define AS_CLASS(value) ((ObjClass*)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_INSTANCE(value) ((ObjInstance*)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative*)AS_OBJ(value))->function)
// These macros take a Value that is expected to have a pointer to a valid ObjString
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_BOUND_METHOD,
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_INSTANCE,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE
} ObjType;

/* Maps the enum value to a string. */
static const char* objTypeAsString[] = {
    "closure",
    "function",
    "native",
    "string",
    "upvalue",
};

struct Obj {
    ObjType type;
    bool isMarked;
    struct Obj* next;  // Intrusive pointer to next object for GC
};

// Functions are first class.
typedef struct {
    Obj obj;
    int arity;
    int upvalueCount;
    Chunk chunk;
    ObjString* name;
} ObjFunction;

// Native functions need to be "their own thing" because they don't push a CallFrame when called.
typedef Value (*NativeFn)(int argCount, Value* args);
typedef struct {
    Obj obj;
    NativeFn function;
} ObjNative;

// ObjString can be cast to Obj, which allows for a form of OOP.
// This is because C struct fields are arranged in memory in the order that they are declared
struct ObjString {
    Obj obj;     // state all Objs share
    int length;  // length without \0
    char* chars;
    uint32_t hash;
};

typedef struct ObjUpvalue {
    Obj obj;
    Value* location;
    Value closed;             // If this opvalue is closed (i.e. the variable it points to is no longer on the stack), we use this 'closed' Value to keep the variable in the heap.
    struct ObjUpvalue* next;  // intrusive, sorted linked list to keep track of all upvalues. The "next" ObjUpvalue is the one that references a local variable farther down (or left) the stack.
} ObjUpvalue;

// A closure wraps every ObjFunction and captures runtime state.
typedef struct {
    Obj obj;
    ObjFunction* function;
    ObjUpvalue** upvalues;
    int upvalueCount;
} ObjClosure;

typedef struct {
    Obj obj;
    ObjString* name;
    Table methods;
} ObjClass;

typedef struct {
    Obj obj;
    ObjClass* loxClass;
    Table fields;
} ObjInstance;

/**
 * A "bound method" is an object that represents a method that is bound to instance of a class.
 * When a method is accessed, the interpreter creates a bound method object that combines the instance (receiver) and the closure (code that defines the method).
 * The bound method can later be called as a function and, when invoked, the VM makes sure that 'this' refers to the correct instance the method was accessed from. */
typedef struct {
    Obj obj;
    Value receiver;  // Instance (as Value so we avoid conversions)
    ObjClosure* method;
} ObjBoundMethod;

ObjBoundMethod* newBoundMethod(Value receiver,
                               ObjClosure* method);
ObjClass* newClass(ObjString* name);
ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjInstance* newInstance(ObjClass* loxClass);
ObjNative* newNative(NativeFn function);
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
ObjUpvalue* newUpvalue(Value* slot);
void printObject(Value value);

// We use a function outside of the macro to provent expanding `value` twice
static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif