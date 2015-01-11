#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uintptr_t T;
typedef void *CP;
typedef struct frame{struct frame *f;CP cp;T *V;T NV;T *r;}frm_t;
typedef struct fun{void *pa;T na;T nv;}fn_t;
typedef struct mw{T t;T c;}mw_t;
typedef struct list{T t;T c;T hd;T tl;}list_t;
typedef struct tuple{T t;T c;T n;T e[];}tuple_t;
enum tag{TI=1,TA=2,TP=3,TF=4,TN=5,TM=7};
enum type{TD=15,TT=23,TL=31};

#define RT return
T MK(T x,T t){RT (x<<3)|t;}
T I(T x){RT MK(x,TI);}
T A(T x){RT MK(x,TA);}
T P(T x){RT MK(x,TP);}
T F(T x){RT MK(x,TF);}
T INT(T x){RT x>>3;}
T TAG(T x){RT x&7;}
T MT(T x){RT *(T*)x;}
void X(){fprintf(stderr,"badmatch\n");exit(1);}
#define PL(x)((list_t *)x)
#define PT(x)((tuple_t *)x)
#define PM(x)((mw_t *)x)
#define IFNT(x,t)if(TAG(x)!=(t))
#define IFNMT(x,t)if(TAG(x)||(MT(x)!=(t)))
T False=(0<<3)|TA;
T True=(1<<3)|TA;
void *alloc(T s){void *p;posix_memalign(&p,8,s);memset(p,0,s);RT p;}
#define SZ(t)sizeof(t)
#define NEW(v,t,n)t *v=alloc(SZ(t)*(n));
#define M(x,y)move(&(y),(x));
#define J(l)goto*&&l;
#define B(v,l)if((v)==False)goto*&&l;
#define R(v)goto*pop(v);
#define FN(n,a,b,c)FUN[n].pa=&&a;FUN[n].na=b;FUN[n].nv=c;
#define CP(l)c##l
#define CALL_CHT(l,r,f,...)goto*call((&r),(f),&&CP(l),##__VA_ARGS__);CP(l):;
#define C(r,f,...)switch(TAG(f)){                               \
  case TP:M(BIF[INT(f)](__VA_ARGS__),(r));break;                \
  case TF:CALL_CHT(__COUNTER__,(r),INT(f),##__VA_ARGS__);break; \
  default:X();}                                                 \

extern fn_t FUN[];
frm_t *S=NULL; T *V=NULL; T NV=0;

void dealloc(T x);
T mw(T x){return TAG(x)||!x;}
void incref(T x){if(mw(x))RT;PM(x)->c++;}
void decref(T x){if(mw(x))RT;PM(x)->c--;if(!(PM(x)->c))dealloc(x);}
void move(T*y,T x){incref(x);decref(*y);*y=x;}
void *pop(T r) {
  frm_t *f=S; CP cp=f->cp;
  move(f->r,r);
  S=f->f;
  T i;
  for(i=0;i<NV;i++)
    decref(V[i]);
  free(V);
  NV=f->NV;
  V=f->V;
  free(f);
  RT cp;
}
void *call(T *r,T n,CP cp,...) {
  NEW(f,frm_t,1);
  f->f=S; f->cp=cp; f->V=V; f->NV=NV; f->r=r; S=f;
  NV=FUN[n].nv;
  V=alloc(SZ(T)*NV);
  T i; va_list ap;
  va_start(ap,FUN[n].na);
  for(i=0;i<FUN[n].na;i++)
    M(va_arg(ap,T),V[i]);
  va_end(ap);
  RT FUN[n].pa;
}
void dealloc(T x){
  if(mw(x))X();
  switch(MT(x)){
  case TT:{
    T i;
    for(i=0;i<PT(x)->n;i++)
      decref(PT(x)->e[i]);
    free(PT(x));
    return;
  }
  case TL:
    decref(PL(x)->hd);
    decref(PL(x)->tl);
    free(PL(x));
    return;
  default: X();
  }
}
