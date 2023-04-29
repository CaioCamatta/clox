#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "../../src/memory.h"
#include "../../src/object.h"
#include "../../src/table.h"
#include "../../src/value.h"
#include "../../src/vm.h"

#define START_CLOCK clock_t start_time = clock();
#define END_CLOCK printf("%s: %.8f seconds\n", __func__, (double)(clock() - start_time) / CLOCKS_PER_SEC);

char* random_string(int length) {
    char* str = malloc(length + 1);
    const char charset[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

    srand(time(NULL));

    for (int i = 0; i < length; i++) {
        str[i] = charset[rand() % (sizeof(charset) - 1)];
    }

    str[length] = '\0';

    return str;
}

static void benchmarkTable() {
    START_CLOCK;

    for (size_t i = 1; i < 30000000; i++) {
        char* chars = random_string(6);
        takeString(chars, 1);
    }

    END_CLOCK;
}

int main(int argc, const char* argv[]) {
    initVM();
    benchmarkTable();
    freeVM();
    return 0;
}
