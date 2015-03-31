T std_is_tuple(T x){
  if (!IS_POINTER(x)) BADMATCH;
  if (PTAG(x) == TAG_TUPLE)
    return a_true;
  return a_false;
}
