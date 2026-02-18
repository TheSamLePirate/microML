typedef struct MLV MLV;

typedef MLV (*ml_fun)(MLV clos, MLV arg);

struct MLV {
  int value;
  ml_fun funptr;
  int size;
  struct MLV *ptr;
};

struct MLV clot_call(struct MLV a, int n, ...);
struct MLV clot_make(void* funptr, int arr, int n, ...);

struct MLV construct(int f, struct MLV a);
struct MLV destruct(struct MLV a);
int is_constructed(int f, struct MLV a);

struct MLV tuple_nth(int i, struct MLV a);
struct MLV make_tuple(int i, ...);

struct MLV make_int(int i);
struct MLV int_add(struct MLV a, struct MLV b);
struct MLV int_sub(struct MLV a, struct MLV b);
struct MLV int_mul(struct MLV a, struct MLV b);
struct MLV int_div(struct MLV a, struct MLV b);
