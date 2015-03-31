T std_make_tuple(T x,...){
  if (TAG(x) != TAG_INT)
    BADMATCH;
  T n = VALUE(x);

  struct tuple *tuple = (struct tuple *)term_alloc(sizeof(struct tuple) + sizeof(T) * n);
  term_set_tag(tuple, TAG_TUPLE);
  tuple->n = n;
  T i;
  va_list ap;
  va_start(ap,n);
  for(i=0; i<n; i++) {
    tuple->elements[i] = va_arg(ap, T);
  }
  va_end(ap);
  return (T)tuple;
}
