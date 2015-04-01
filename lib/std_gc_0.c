T std_gc_0() {
    gc_mark();
    gc_sweep();
    return (T)NULL;
}
