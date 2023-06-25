#include "memory.h"

#include <stdlib.h>

#include "compiler.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>

#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

/**
 * Reallocate an array, creating/growing/shrinking/freeing space if needed
 *
 * Note: All we need to update a memory block is the first byte. The memory allocator maintains additional bookkeeping info for each block in the heap (incl size).
 */
void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    // Keep track of how many bytes we're allocating to help us decide when to run GC.
    vm.bytesAllocated += newSize - oldSize;

    // GC itself calls reallocate, so don't trigger when freeing memory .
    if (newSize > oldSize) {
        // If defined, trigger GC at every moment for testing
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#endif

        if (vm.bytesAllocated > vm.nextGC) {
            collectGarbage();
        }
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

/* Mark object for GC */
void markObject(Obj* object) {
    if (object == NULL) return;
    if (object->isMarked) return;  // Prevents looping on cycles

#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    object->isMarked = true;

    // Mark objects as gray
    if (vm.grayCapacity < vm.grayCount + 1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        // Use system realloc instead of our own reallocate so we can't trigger a GC while doing a GC.
        vm.grayStack = (Obj**)realloc(vm.grayStack,
                                      sizeof(Obj*) * vm.grayCapacity);
        // Allocation failed
        if (vm.grayStack == NULL) exit(1);
    }

    vm.grayStack[vm.grayCount++] = object;
}

/* Mark value for GC */
void markValue(Value value) {
    // We only care about Values with heap allocation. (i.e. not numbers, bools, nil).
    if (IS_OBJ(value)) markObject(AS_OBJ(value));
}

/* Mark all values in an (ObjFunction's) array of values. */
static void markArray(ValueArray* array) {
    for (int i = 0; i < array->count; i++) {
        markValue(array->values[i]);
    }
}

/**
 * Recursively blacken objects.
 *
 * A object is black if its isMarked field is set but it is no longer in the gray stack
 */
static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    switch (object->type) {
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            markObject((Obj*)closure->function);
            for (int i = 0; i < closure->upvalueCount; i++) {
                markObject((Obj*)closure->upvalues[i]);
            }
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            markObject((Obj*)function->name);
            markArray(&function->chunk.constants);
            break;
        }
        case OBJ_UPVALUE:
            markValue(((ObjUpvalue*)object)->closed);
            break;
        // These have no outgoing references
        // TODO(optimization): don't mark these are gray in the first place. Just go straight to black.
        case OBJ_NATIVE:
        case OBJ_STRING:
            break;
    }
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

/* Mark all reachable roots */
static void markRoots() {
    // Most roots are locals/temporaries in the stack
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }

    // Closures in CallFrames
    for (int i = 0; i < vm.frameCount; i++) {
        markObject((Obj*)vm.frames[i].closure);
    }

    // Upvalues are also directly reachable
    for (ObjUpvalue* upvalue = vm.openUpvalues;
         upvalue != NULL;
         upvalue = upvalue->next) {
        markObject((Obj*)upvalue);
    }

    // Then, globals
    markTable(&vm.globals);

    // We deliberately don't mark vm.strings. The interned strings are handled differently, using weak references.

    // GC can also run during compiling
    markCompilerRoots();
}

/* Traverse stack of grays until all are black. */
static void traceReferences() {
    while (vm.grayCount > 0) {
        Obj* object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}

/* Delete all white objects. */
static void sweep() {
    Obj* previous = NULL;
    Obj* object = vm.objects;

    while (object != NULL) {
        if (object->isMarked) {
            object->isMarked = false;  // For next GC cycle
            previous = object;
            object = object->next;
        } else {
            // Removed unreached object from middle of linked list
            Obj* unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm.objects = object;
            }

            freeObject(unreached);
        }
    }
}

/* Mark-and-sweep garbage collection. */
void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
    size_t before = vm.bytesAllocated;
#endif

    markRoots();
    traceReferences();
    tableRemoveWhite(&vm.strings);
    sweep();

    // After freeing objects, we determine that the next GC cycle will run once memory has grown by a certain factor
    vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
    printf("-- gc end\n");
    printf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
           before - vm.bytesAllocated, before, vm.bytesAllocated,
           vm.nextGC);
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

    free(vm.grayStack);
}