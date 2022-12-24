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
struct ht_string {
  struct kv_string data[16];
  int count;
};

struct ht_string ht_string_new() {
  struct ht_string* new = malloc(sizeof(struct ht_string));
  new->count = 0;

  return *new;
}

void ht_print(struct ht_string* ht) {
  struct kv_string* kv;
  printf("{\n");
  for(int i = 0; i < ht->count; i++) {
    kv = &ht->data[i];
    printf("\t%s: %s,\n", kv->key, kv->val);
  }
  printf("}\n");
}

struct kv_string* ht_find(struct ht_string* ht, char* in) {
  for (int i = 0; i < ht->count; i++) {
    int ret = strcmp(ht->data[i].key, in);
    if (ret == 0) {
      return &ht->data[i];
    }
  }

  return NULL;
}

void ht_set(struct ht_string* ht, char* key, char* val) {
  struct kv_string* found = ht_find(ht, key);
  if (found != NULL) { // update
    strcpy(found->key, key);
    strcpy(found->val, val);
    return;
  }

  // make new
  found = &ht->data[ht->count];
  strcpy(found->key, key);
  strcpy(found->val, val);
  ht->count++;

  return;
}

char* ht_get(struct ht_string* ht, char* in) {
  struct kv_string* found = ht_find(ht, in);
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

  struct ht_string ht = ht_string_new();

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

    ht_set(&ht, argv[i], argv[i+1]);
  }

  ht_print(&ht);

  printf("Get the val for key 'abc': %s\n", ht_get(&ht, "abc"));
}
