// gcc -g -Wall -o app app.c ringbuffer.c

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ringbuffer.h"


int
main(){
    RBuf* rb = rbuf_new(5);

    printf("push 3\n");
    rbuf_push(rb, "foo");
    rbuf_push(rb, "bar");
    rbuf_push(rb, "baz");
    rbuf_print(rb);
    printf("push 3\n");
    rbuf_push(rb, "qux");
    rbuf_push(rb, "quux");
    rbuf_push(rb, "foo2");
    rbuf_print(rb);
    printf("pop 2\n");
    printf("popped=%s\n", rbuf_pop(rb));
    printf("popped=%s\n", rbuf_pop(rb));
    rbuf_print(rb);
    printf("push 1\n");
    rbuf_push(rb, "bif");
    rbuf_print(rb);
}
