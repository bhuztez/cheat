T std_is_cons_1(T x){
  if (!IS_POINTER(x)) BADMATCH;
  if (PTAG(x) == TAG_LIST)
    return a_true;
  return a_false;
}
