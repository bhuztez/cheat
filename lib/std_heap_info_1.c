T std_heap_info_1(T x) {
    switch (x) {
    case a_size:
        return I(default_pool.pages * PAGE_SIZE);
    case a_free:
	return I(default_pool.free_size * sizeof(T));
    case a_used:
	return I(default_pool.pages * PAGE_SIZE - default_pool.free_size * sizeof(T));
    default:
        BADMATCH;
    }
}
