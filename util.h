#ifndef __UTIL_H__
#define __UTIL_H__
#include <stdint.h>
#include <assert.h>
#include <stdio.h>

#define _DEBUG_LOC3(f, l) f ":" #l
#define _DEBUG_LOC2(f, l) _DEBUG_LOC3(f, l)
#define _DEBUG_LOC _DEBUG_LOC2(__FILE__, __LINE__)

#define DBG(...) do { fprintf(stderr, _DEBUG_LOC " DBG: " __VA_ARGS__); fprintf(stderr, "\n"); } while(0)

#define LOG(...) do { fprintf(stderr, _DEBUG_LOC " LOG: " __VA_ARGS__); fprintf(stderr, "\n"); } while(0)

#define ASSERT(x) assert(x)

typedef uint64_t u64;
typedef uint32_t u32;
typedef int64_t i64;
typedef int32_t i32;

template<typename T>
T *mem_realloc(T *buf, u64 items) {
    return static_cast<T*>(realloc(buf, sizeof(T) * items));
}
#endif // __UTIL_H__
