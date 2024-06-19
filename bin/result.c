#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdatomic.h>

static char* array;

void set(uint32_t i, uint32_t val) {
    if (array == NULL)
        array = malloc(0xffffffff);
    array[i] = val;
}

enum {
    CHUNK = 8192,
};

void write_all() {
    fprintf(stderr, "writing...\n");
    size_t total = 0;
    for (size_t i = 0; i < 0xffffffffUL+1; i += CHUNK) {
        total += write(fileno(stdout), &array[i], CHUNK);
    }
    fprintf(stderr, "%ld\n", total);
}
 
_Atomic(uint64_t) count;

void inc() {
    count++;
    if (count % 1000000 == 0)
        fprintf(stderr, "%.2f\n", (float) count / (float) 0xffffffffULL * 100);
}
