T std_binary_at_2(T b, T n) {
  if ((PTAG(b) != TAG_SUBBIN) || (TAG(n) != TAG_INT))
    BADMATCH;

  struct binary *bin = PSUBBIN(b)->bin;
  T i = VALUE(n);
  T offset = PSUBBIN(b)->offset;

  if (i >= PSUBBIN(b)->length)
    BADMATCH;

  if ((offset+i) >= bin->n)
    BADMATCH;

  return I(bin->data[offset+i]);
}
