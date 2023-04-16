#include "table.h"

#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

/* Take a key and an array of "buckets" (entries) and find which bucket the key belongs to. Returns a pointer to the bucket. */
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    uint32_t index = key->hash % capacity;

    Entry* tombstone = NULL;

    // Linear probing and collision handling
    // Assumes we will eventually hit an empty bucket
    for (;;) {
        Entry* entry = &entries[index];

        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                // Empty entry
                // If we've passed a tombstone while looking for the (emptry) entry, we return that bucket instead of the later empty one.
                // That way if we're calling findEntry to insert a node, we can reuse the tombstone.
                return tombstone != NULL ? tombstone : entry;
            } else {
                // We found a tombstone
                // Save its location and continue loop.
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (entry->key == key) {
            // We found the key
            return entry;
        }

        index = (index + 1) % capacity;
    }
}

/* Given a table and a key, copy value to output parameter 'value'. Returns true if value exists and was copied, and false if it does not find the key.  */
bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    *value = entry->value;
    return true;
}

/* Allocate array for table entries and initialize Entry's to be NULL: NIL_VAL */
static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity);
    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // When updating the size of the array, we need to re-map the entries. We do so by rebuilding the table from scratch and re-inserting all entries.
    table->count = 0;
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL) continue;

        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }
    // Release the memory for the old array.
    FREE_ARRAY(Entry, table->entries, table->capacity);

    table->entries = entries;
    table->capacity = capacity;
}

/* Add given key-value pair to the given hash table. If an entry is present, the new value will overwrite the old value. Returns true if a new entry was added. */
bool tableSet(Table* table, ObjString* key, Value value) {
    // Grow the array if it becomes at least TABLE_MAX_LOAD % full (75% full).
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }
    // entry is the "bucket"
    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;
    // Only add count if we are not replacing a tombstonel
    if (isNewKey && IS_NIL(entry->value)) table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

/* Delete an entry in a hash table. Under the hood, this funtion places a tombstone. Returns true if the entry was delete, and false if the entry was not found. */
bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0) return false;

    // Find the entry.
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    // Place a tombstone in the entry.
    entry->key = NULL;
    entry->value = BOOL_VAL(true);  // tombstone
    // note: we dont reduce count -- tombstones count towards the load factor.
    return true;
}

/* Copy all entries of one hash table into another */
void tableAddAll(Table* from, Table* to) {
    for (int i = 0; i < from->capacity; i++) {
        Entry* entry = &from->entries[i];
        if (entry->key != NULL) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

/* Look for a string on the table. This is an alternative to 'findEntry' that correctly compares strings character-by-character instead of by reference. */
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if (table->count == 0) return NULL;

    uint32_t index = hash % table->capacity;
    for (;;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            // Stop if we find an empty non-tombstone entry.
            if (IS_NIL(entry->value)) return NULL;
        } else if (entry->key->length == length &&
                   entry->key->hash == hash &&
                   memcmp(entry->key->chars, chars, length) == 0) {
            // If the length, hash, and values are the same, we found it.
            return entry->key;
        }

        index = (index + 1) % table->capacity;
    }
}
