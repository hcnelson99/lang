#ifndef __VEC_H__
#define __VEC_H__
#include <stdlib.h>

#include "util.h"

template <typename T>
struct Vec {
    static Vec<T> empty() {
        Vec<T> v;
        v.buf = NULL;
        v.length = 0;
        v.capacity = 0;
        return v;
    }

    void reserve(u64 items) {
        while (length + items > capacity) {
            capacity = capacity == 0 ? 1 : capacity * 2;
            buf = mem_realloc(buf, capacity);
        }
    }

    void add(T item) {
        reserve(1);
        ASSERT(length <= capacity);
        buf[length++] = item;
    }

    T& operator[](u64 i) {
        ASSERT(i < length);
        ASSERT(length <= capacity);
        return buf[i];
    }

    const T& operator[](u64 i) const {
        ASSERT(i < length);
        ASSERT(length <= capacity);
        return buf[i];
    }

    T *buf;
    u64 length;
    u64 capacity;
};

#define For(v) for (auto it = v.buf; it != v.buf + v.length; ++it)

#endif // __VEC_H__
