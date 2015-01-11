T make_tuple(T x,...){
IFNT(x,TI)X();
T n=INT(x);
tuple_t *t=(tuple_t *)alloc(SZ(tuple_t)+SZ(T)*n);
t->t = TT;t->n = n;
T i;
va_list ap;
va_start(ap,n);
for(i=0;i<n;i++)M(va_arg(ap,T),t->e[i]);
va_end(ap);
RT(T)t;
}
