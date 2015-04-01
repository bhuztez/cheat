T std_element_2(T x,T y){
  if (TAG(x) != TAG_INT)
    BADMATCH;
  if (!IS_POINTER(y))
    BADMATCH;
  if (PTAG(y) != TAG_TUPLE)
    BADMATCH;

  T i = VALUE(x);
  if ((i < 1) || (i > (PTUPLE(y)->n)))
    BADMATCH;

  return PTUPLE(y)->elements[i-1];
}
