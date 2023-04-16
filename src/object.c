#include "object.h"

#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

/* Allocate obj of the given size on the heap.
    size = the number of bytes so that there's room for the extra payload fields needed for the specific obj type being created. */
static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;

    // every time we allocate an obj, update the global lsit of objects.
    // this is useful for GC
    object->next = vm.objects;
    vm.objects = object;
    return object;
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
    return allocateString(chars, length, hash);
}

/* Copy a string to heap */
ObjString* copyString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    char* heapChars = ALLOCATE(char, length + 1);
    // Copy string from the lexeme (in the monolithic source string)
    memcpy(heapChars, chars, length);
    // Terminate string to facilitate conversion to c string
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
    }
}