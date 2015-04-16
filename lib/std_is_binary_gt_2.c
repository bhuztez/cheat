T std_is_binary_gt_2(T b, T n) {
  if (TAG(n) != TAG_INT)
    BADMATCH;

  if (!IS_POINTER(b))
    BADMATCH;

  if (PTAG(b) != TAG_SUBBIN)
    return a_false;

  if (VALUE(n) > PSUBBIN(b)->length)
    return a_false;
  return a_true;
}
