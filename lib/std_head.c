T std_head(T x){
  if (!IS_POINTER(x)) BADMATCH;
  if (PTAG(x) != TAG_LIST) BADMATCH;
  return PLIST(x)->hd;
}
