#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"

#define GROW_CAPACITY(capacity) \ 
    ((capacity) < 8 ? 8 : (capacity)*2)

// This macro takes care of getting the size of the array's element type
// & casting the resulting void* back to a pointer of the right type.
#define GROW_ARRAY(type, pointer, oldCount, newCount) \ 
    (type*)reallocate(pointer, sizeof(type) * (oldCount), sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \ 
    reallocate(pointer, sizeof(type) * (oldCount), 0)

#endif