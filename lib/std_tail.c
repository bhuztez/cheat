T std_tail(T x){
  if (!IS_POINTER(x)) BADMATCH;
  if (PTAG(x) != TAG_LIST) BADMATCH;
  return PLIST(x)->tl;
}
