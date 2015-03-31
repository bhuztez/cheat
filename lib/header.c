enum tag {
  TAG_INT  = 0b0001,
  TAG_ATOM = 0b0011,
  TAG_NIF  = 0b0101,
  TAG_FUN  = 0b0111,
  TAG_FRAME,
  TAG_TUPLE,
  TAG_LIST,
};

#define TAG(X) ((X)&0b1111)
#define VALUE(X) ((X)>>4)
#define IS_POINTER(X) (((X)&0b1)==0)
#define PTAG(X) term_get_tag((void *)(X))
#define PFRAME(X) ((struct frame *)(X))
#define PLIST(X) ((struct list *)(X))
#define PTUPLE(X) ((struct tuple *)(X))

#define I(X) (((X)<<4)|TAG_INT)
#define A(X) (((X)<<4)|TAG_ATOM)
#define N(X) (((X)<<4)|TAG_NIF)
#define F(X) (((X)<<4)|TAG_FUN)
#define S(X) (((X)<<4)|0b1001)
#define RET(X) goto *pop_frame((X));
#define BR(X) if ((X) == a_false) goto
#define V(X) (stack->vars[(X)])
#define BADMATCH badmatch(__FUNCTION__)

#define CP(l) c##l

#define CALL(l,r,f,...)                         \
  goto *call(&&CP(l),&(r),(f),##__VA_ARGS__);   \
CP(l):;                                         \

#define C(r, f, ...)                            \
  if(TAG(f)==TAG_NIF) {                         \
    r = N[VALUE(f)](__VA_ARGS__);               \
  } else {                                      \
    CALL(__COUNTER__,(r),(f),##__VA_ARGS__);    \
  }                                             \

void badmatch(char const *s) __attribute__((noreturn));
void badmatch(char const *s) {
  fprintf(stderr, "badmatch %s\n", s);
  exit(1);
}

struct frame {
  struct frame *next_frame;
  void *cp;
  T *result;
  T n;
  T vars[];
};

struct fun {
  void *addr;
  T nargs;
  T nvars;
};

struct list {
  T hd;
  T tl;
};

struct tuple {
  T n;
  T elements[];
};

struct frame *stack = NULL;
struct fun *F = NULL;

void *call(void *cp, T *result, T f, ...) {
  if (TAG(f)==TAG_FUN) {
    f = VALUE(f);
    struct frame *frame = (struct frame *)term_alloc(sizeof(struct frame) + sizeof(T)*(F[f].nvars));
    term_set_tag(frame, TAG_FRAME);
    frame->next_frame = stack;
    frame->cp = cp;
    frame->result = result;
    frame->n = F[f].nvars;
    T i;
    va_list(ap);
    va_start(ap, F[f].nargs);
    for(i=0; i<F[f].nargs; i++) {
      frame->vars[i] = va_arg(ap, T);
    }
    va_end(ap);
    stack = frame;
    return F[f].addr;
  } else {
    BADMATCH;
  }
}

void *pop_frame(T result) {
  void *cp = stack->cp;
  *(stack->result) = result;
  stack = stack->next_frame;
  return cp;
}

static T const nil = A(0);

void gc_mark() {
  void *terms[default_pool.free_blocks * 2 + 1];
  int top = -1;
  void visit_term(T t) {
    if (!IS_POINTER(t)) return;
    void *p = (void *)t;
    if (p == NULL) return;
    if (term_get_mark(p)) return;
    term_set_mark(p);
    top += 1;
    terms[top] = p;
  }
  visit_term((T)stack);
  while (top >= 0) {
    void *current = terms[top];
    top -= 1;
    T i;
    switch (term_get_tag(current)) {
    case TAG_FRAME:
      for(i=0;i<PFRAME(current)->n;i++) {
        visit_term(PFRAME(current)->vars[i]);
      }
      break;
    case TAG_LIST:
      visit_term(PLIST(current)->hd);
      visit_term(PLIST(current)->tl);
      break;
    case TAG_TUPLE:
      for(i=0;i<PTUPLE(current)->n;i++) {
        visit_term(PTUPLE(current)->elements[i]);
      }
      break;
    default:
      BADMATCH;
    }
  }
};
