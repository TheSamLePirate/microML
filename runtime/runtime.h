typedef struct MLV MLV;

typedef MLV (*ml_fun)(MLV clos, MLV arg);

struct MLV {
  int value;
  ml_fun funptr;
  int size;
  struct MLV *ptr;
};
