#include "memory.h"

#include <stdlib.h>

#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>

#include "debug.h"
#endif

/**
 * Reallocate an array, creating/growing/shrinking/freeing space if needed
 *
 * Note: All we need to update a memory block is the first byte. The memory allocator maintains additional bookkeeping info for each block in the heap (incl size).
 */
void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    // GC itself calls reallocate, so don't trigger when freeing memory .
    if (newSize > oldSize) {
        // If defined, trigger GC at every moment for testing
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#endif
    }

    // If we empty the array, we deallocate
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    // Grow/shrink/allocate
    // Realloc is equivalent to malloc when oldSIze is zero
    void* result = realloc(pointer, newSize);
    if (result == NULL) exit(1);
    return result;
}

// Free an object
static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void*)object, object->type);
#endif

    switch (object->type) {
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(ObjUpvalue*, closure->upvalues,
                       closure->upvalueCount);
            FREE(ObjClosure, object);
            // We don't free the function in the closure because the closure doesn't own the function
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
            break;
        }
        case OBJ_NATIVE:
            FREE(ObjNative, object);
            break;
        case OBJ_STRING: {
            ObjString* string = (ObjString*)object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
        case OBJ_UPVALUE:
            FREE(ObjUpvalue, object);
            break;
        default:
            break;
    }
}

/* Mark-and-sweep garbage collection. */
void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
#endif

#ifdef DEBUG_LOG_GC
    printf("-- gc end\n");
#endif
}

// Free all objects in the VM
void freeObjects() {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }
}