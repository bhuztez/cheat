T std_binary_pos_3(T b, T x, T y) {
  if ((PTAG(b) != TAG_SUBBIN) || (TAG(x) != TAG_INT))
    BADMATCH;

  if ((y != a_infinity) && (TAG(y) != TAG_INT))
    BADMATCH;

  struct subbin *sb = PSUBBIN(b);
  struct binary *bin = sb->bin;
  T offset = sb->offset + VALUE(x);
  if (offset > bin->n)
    BADMATCH;
  T length = (y==a_infinity)?(bin->n - offset):VALUE(y);
  if (offset + length > bin->n)
    BADMATCH;

  struct subbin *sb1 = term_alloc(sizeof(struct subbin));
  term_set_tag(sb1, TAG_SUBBIN);
  sb1->bin = sb->bin;
  sb1->offset = offset;
  sb1->length = length;

  return (T)sb1;
}
