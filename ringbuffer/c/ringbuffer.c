#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "ringbuffer.h"

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
