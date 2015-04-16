T std_match_2(T x, T y) {
  if (x==y)
    return a_true;
  if ((!IS_POINTER(x)) || (!IS_POINTER(y)))
    return a_false;
  if (PTAG(x) != PTAG(y))
    return a_false;
  switch (PTAG(x)) {
  case TAG_LIST:
    if(std_match_2(PLIST(x)->hd, PLIST(y)->hd) == a_false)
      return a_false;
    return std_match_2(PLIST(x)->tl, PLIST(y)->tl);
  case TAG_TUPLE:
    if (PTUPLE(x)->n != PTUPLE(y)->n)
      return a_false;
    T i;
    for(i=0; i<(PTUPLE(x)->n); i++)
      if (std_match_2(PTUPLE(x)->elements[i], PTUPLE(y)->elements[i]) == a_false)
        return a_false;
    return a_true;
  case TAG_SUBBIN:
    if (PSUBBIN(x)->length != PSUBBIN(y)->length)
      return a_false;
    if ((PSUBBIN(x)->bin == PSUBBIN(y)->bin) && (PSUBBIN(x)->offset == PSUBBIN(y)->offset))
      return a_true;

    char * bx = (PSUBBIN(x)->bin->data) + PSUBBIN(x)->offset;
    char * by = (PSUBBIN(y)->bin->data) + PSUBBIN(y)->offset;

    if (strncmp(bx, by, PSUBBIN(x)->length) == 0)
      return a_true;
    return a_false;
  default:
    return a_false;
  }
}
