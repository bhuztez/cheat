T std_match(T x, T y) {
  if (x==y)
    return a_true;
  if ((!IS_POINTER(x)) || (!IS_POINTER(y)))
    return a_false;
  if (PTAG(x) != PTAG(y))
    return a_false;
  switch (PTAG(x)) {
  case TAG_LIST:
    if(std_match(PLIST(x)->hd, PLIST(y)->hd) == a_false)
      return a_false;
    return std_match(PLIST(x)->tl, PLIST(y)->tl);
  case TAG_TUPLE:
    if (PTUPLE(x)->n != PTUPLE(y)->n)
      return a_false;
    T i;
    for(i=0; i<(PTUPLE(x)->n); i++)
      if (std_match(PTUPLE(x)->elements[i], PTUPLE(y)->elements[i]) == a_false)
        return a_false;
    return a_true;   
  default:
    return a_false;
  }  
}
