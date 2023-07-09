#include "object.h"

#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

/* Allocate obj of the given size on the heap.
    size = the number of bytes so that there's room for the extra payload fields needed for the specific obj type being created. */
static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;
    object->numRefs = 0;

    // every time we allocate an obj, update the global lsit of objects.
    // this is useful for GC
    object->next = vm.objects;
    vm.objects = object;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for %s\n", (void*)object, size, objTypeAsString[object->type]);
#endif

    return object;
}

/* Create new cluster that wraps a function. */
ObjClosure* newClosure(ObjFunction* function) {
    // Allocate array of upvalues with correct size. The size was determined at compile time.
    ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*,
                                     function->upvalueCount);
    for (int i = 0; i < function->upvalueCount; i++) {
        upvalues[i] = NULL;
    }

    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalueCount = function->upvalueCount;
    return closure;
}

/* Create new Lox function */
ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalueCount = 0;
    initChunk(&function->chunk);
    function->name = NULL;
    return function;
}

/* Create new native function. Take a C function pointer and wrap it in an ObjNative. */
ObjNative* newNative(NativeFn function) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    return native;
}

/* Create new ObjString on the heap and initialize its fields.
Similar to a constructor in OOP.*/
static ObjString* allocateString(char* chars, int length,
                                 uint32_t hash) {
    // Call the "base class" constructor
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;

    // Protect from GC. Here, the string is brand new, so it's not pointed to by anyone.
    push(OBJ_VAL(string));

    // Intern every string we create
    tableSet(&vm.strings, string, NIL_VAL);
    pop();

    return string;
}

/* FNV-1a hash */
static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (u_int8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

/* Allocate string in heap from C string passed in, essentially claiming ownership of the string given.
This function is used for concatenation.*/
ObjString* takeString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);

    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        // If the string we're "taking" is already in the list of interned strings, free the string that was passed in and use the interned one.
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }

    return allocateString(chars, length, hash);
}

/* Copy a string to heap */
ObjString* copyString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    // If a string is already in the list of interned strings, instead of copying it we just return a reference.
    // Interning all strings allow the VM to take for granted that any two strings at different addresses have different contents.
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) return interned;

    char* heapChars = ALLOCATE(char, length + 1);
    // Copy string from the lexeme (in the monolithic source string)
    memcpy(heapChars, chars, length);
    // Terminate string to facilitate conversion to c string
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
}

/* Takes address of the slot where the closed-over variable lives and creates a runtime upvalue object. */
ObjUpvalue* newUpvalue(Value* slot) {
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->closed = NIL_VAL;
    upvalue->location = slot;
    upvalue->next = NULL;
    return upvalue;
}

/* Print the name of a function */
static void printFunction(ObjFunction* function) {
    if (function->name == NULL) {
        printf("<script>");
        return;
    }
    printf("<fn %s>", function->name->chars);
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_CLOSURE:
            printFunction(AS_CLOSURE(value)->function);
            break;
        case OBJ_FUNCTION:
            printFunction(AS_FUNCTION(value));
            break;
        case OBJ_NATIVE:
            printf("<native fn>");
            break;
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
        case OBJ_UPVALUE:
            printf("upvalue");
            break;
    }
}