/*
gcc -o ht_string ht.c

This implements a basic associative array using a simple char-array for both
string keys and values.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct kv_string {
  char key[100];
  char val[100];
};

void kvs_print(struct kv_string* kvs, unsigned int count) {
  struct kv_string kv;
  printf("{\n");
  for(int i = 0; i < count; i++) {
    kv = kvs[i];
    printf("\t%s: %s,\n", kv.key, kv.val);
  }
  printf("}\n");
}

struct kv_string* kvs_find(struct kv_string* kvs, int count, char* in) {
  for (int i = 0; i < count; i++) {
    int ret = strcmp(kvs[i].key, in);
    if (ret == 0) {
      return &kvs[i];
    }
  }

  return NULL;
}

void kvs_set(struct kv_string* kvs, int count, char* key, char* val) {
  struct kv_string* found = kvs_find(kvs, count, key);
  if (found != NULL) { // update
    strcpy(found->key, key);
    strcpy(found->val, val);
    return;
  }

  // make new
  found = &kvs[count];
    strcpy(found->key, key);
    strcpy(found->val, val);

  return;
}

char* kvs_get(struct kv_string* kvs, int count, char* in) {
  struct kv_string* found = kvs_find(kvs, count, in);
  if (found != NULL) {
    return found->val;
  }

  return "NOT FOUND";
}

int main(int argc, char* argv[]) {
  if ((argc < 3) || (argc % 2 != 1)) {
    printf("Usage: ./ht_string k1 v1 k2 v2\n");
    exit(1);
  }

  struct kv_string kvs[16];
  int count = 0;

  for (int i = 1; i < argc; i+= 2) {
    int len = strlen(argv[i]);
    if (len > 99) {
      printf("Too long:\n%s\n%s %d\n", argv[i], __FILE__, __LINE__);
      exit(2);
    }

    len = strlen(argv[i+1]);
    if (len > 99) {
      printf("Too long:\n%s", argv[i+1]);
      exit(2);
    }

    kvs_set(kvs, count, argv[i], argv[i+1]);

    count++;
  }

  kvs_print(kvs, count);

  printf("Get the val for key 'abc': %s\n", kvs_get(kvs, count, "abc"));
}
