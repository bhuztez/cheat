T std_head_1(T x){
  if (!IS_POINTER(x)) BADMATCH;
  if (PTAG(x) != TAG_LIST) BADMATCH;
  return PLIST(x)->hd;
}
