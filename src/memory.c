#include "memory.h"

#include <stdlib.h>

/**
 * Reallocate an array, creating/growing/shrinking/freeing space if needed
 *
 * Note: All we need to update a memory block is the first byte. The memory allocator maintains additional bookkeeping info for each block in the heap (incl size).
 */
void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
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