#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uintptr_t T;
typedef void * P;
typedef struct frame {struct frame *f;P cp;T *V;T *r;} frm_t;
typedef struct fun {void *pa;T na;T nv;} fn_t;
enum tag {TI=1,TA=2,TB=3,TF=4};

#define RT return
T TAG(T x,T t){RT (x<<5)|t;}
T INT(T x){RT TAG(x,TI);}
T A(T x){RT TAG(x,TA);}
T BF(T x){RT TAG(x,TB);}
T FN(T x){RT TAG(x,TF);}
T RAW(T x){RT x>>5;}
T TYPE(T x){RT x&0x1f;}
void E(){fprintf(stderr,"badmatch\n");exit(1);}

#define ALLOC(t,n)malloc(sizeof(t)*(n));
#define M(x,y)((y)=(x));
#define J(l)goto*&&l;
#define B(v,l)if((v)==A(0))goto*&&l;
#define R(v)goto*pop(v);
#define F(n,a,b,c)FUN[n].pa=&&a;FUN[n].na=b;FUN[n].nv=c;
#define CP(l)c##l
#define CALL_CHT(l,r,f,...)goto*call((&r),(f),&&CP(l),##__VA_ARGS__);CP(l):;
#define C(r,f,...)switch(TYPE(f)){                              \
  case TB:(r)=BIF[RAW(f)](__VA_ARGS__);break;                   \
  case TF:CALL_CHT(__COUNTER__,(r),RAW(f),##__VA_ARGS__);break; \
  default:E();}                                                 \

extern fn_t FUN[];
frm_t *S=NULL; T *V=NULL;
void *pop(T r) {
  frm_t *f=S; P cp=f->cp; *(f->r)=r;
  S=f->f; free(V); V=f->V; free(f);
  RT cp;
}
void *call(T *r,T n,P cp,...) {
  frm_t *f=ALLOC(frm_t,1);
  f->f=S; f->cp=cp; f->V=V; f->r=r; S=f;
  V=ALLOC(T,FUN[n].nv); T i; va_list ap;
  va_start(ap,FUN[n].na); for(i=0;i<FUN[n].na;i++)V[i]=va_arg(ap,T); va_end(ap);
  RT FUN[n].pa;
}
