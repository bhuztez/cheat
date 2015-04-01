T std_is_tuple_2(T x, T y){
  if (TAG(y) != TAG_INT) BADMATCH;
  if (!IS_POINTER(x)) return a_false;
  if (PTAG(x) != TAG_TUPLE)
    return a_false;
  if (PTUPLE(x)->n == VALUE(y))
    return a_true;
  return a_false;
}
