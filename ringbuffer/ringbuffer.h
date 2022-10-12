typedef struct {
    int size;
    int head;
    int tail;
    char** data;
} RBuf;

RBuf* rbuf_new(int);
void rbuf_push(RBuf*, char*);
void rbuf_print(RBuf*);
char* rbuf_pop(RBuf*);
