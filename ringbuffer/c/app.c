// gcc -o app app.c

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int size;
    int head;
    int tail;
    char** data;
} RBuf;

RBuf*
rbuf_new(int size) {
    RBuf* rb = malloc(sizeof(RBuf));
    rb->size = size;
    rb->head = 0;
    rb->tail = 0;
    rb->data = calloc(size, sizeof(char*));
    for(int i = 0; i<size; i++) {
      rb->data[i] = strdup("");
    }

    return rb;
}

void
rbuf_push(RBuf* rb, char* input) {
  char* i2 = strdup(input);
  char* orig = rb->data[rb->head];
  free(orig);

  rb->data[rb->head] = i2;
  if(rb->head == rb->size-1) {
    rb->head = 0;
  } else {
    rb->head++;
  }
}

void
rbuf_print(RBuf* rb) {
    for(int i = 0; i < rb->size; i++) {
        printf("%s, ", rb->data[i]);
    }
    printf("\n");
}

char*
rbuf_pop(RBuf* rb) {
  char* out = rb->data[rb->tail];

  if (rb->tail == rb->size - 1) {
    rb->tail = 0;
  } else {
    rb->tail++;
  }

  return out;
}

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
