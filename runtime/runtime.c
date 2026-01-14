#include "runtime.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define STEP_SIZE 1024 * 1024
#define INIT_SIZE STEP_SIZE * 128

struct MLV *memory = NULL;
struct MLV *memory_ptr = NULL;
struct MLV *end_memory = NULL;

struct MLV *alloc(int i){
  if (memory == NULL){
    memory = malloc(INIT_SIZE * sizeof(struct MLV));
    memory_ptr = memory;
    end_memory = memory + INIT_SIZE;
  }
  if (end_memory < memory_ptr + i){
    // TODO réalloc
    printf("Pas assez de mémoire.");
    exit(1);
  }
  return memory_ptr += i;
}

struct MLV copy(struct MLV a){
  struct MLV b;
  b.value = a.value;
  b.funptr = a.funptr;
  b.size = a.size;
  b.ptr = alloc(a.size);
  for (int i = 0; i < a.size; i ++){
    b.ptr[i] = a.ptr[i];
  }
  return b;
}

struct MLV clot_call(struct MLV a, struct MLV b) {
  struct MLV cpy = copy(a);
  // TODO
  return cpy;
}

struct MLV make_clot(void* funptr, int arr) {
  struct MLV out;
  out.funptr = funptr;
  out.size = arr;
  out.ptr = alloc(arr);
  out.value = 0;
  return out;
}
void clot_fill(struct MLV *clot, int nth, struct MLV obj){
  clot->ptr[nth] = obj;
}
void clot_set_filled(struct MLV *clot, int v){
  clot->value = v;
}

struct MLV construct(int field, struct MLV v){
  struct MLV out;
  out.ptr = alloc(1);
  out.value = field;
  out.ptr[0] = v;
}
struct MLV record(int nfields){
  struct MLV out;
  out.ptr = alloc(nfields * 2);
  out.size = nfields;
}
void record_fill(struct MLV record, int indice, int field, struct MLV v){
  record.ptr[indice * 2].value = field;
  record.ptr[indice * 2].ptr = NULL;
  record.ptr[indice * 2 + 1] = v;
}
struct MLV record_get(struct MLV record, int field){
  for (int i = 0; i < record.size; i ++){
    if (field == record.ptr[i * 2].value)
      return record.ptr[i * 2 + 1];
  }
  assert(0);
}

// TODO trucs pour le pattern match

struct MLV make_int(int i){
  struct MLV out;
  out.value = i;
  out.ptr = NULL;
  return out;
}

struct MLV int_add(struct MLV a, struct MLV b){
  struct MLV out;
  out.value = a.value + b.value;
  out.ptr = NULL;
  return out;
}

struct MLV int_sub(struct MLV a, struct MLV b){
  struct MLV out;
  out.value = a.value - b.value;
  out.ptr = NULL;
  return out;
}

struct MLV int_mul(struct MLV a, struct MLV b){
  struct MLV out;
  out.value = a.value * b.value;
  out.ptr = NULL;
  return out;
}

struct MLV int_div(struct MLV a, struct MLV b){
  struct MLV out;
  out.value = a.value / b.value;
  out.ptr = NULL;
  return out;
}


