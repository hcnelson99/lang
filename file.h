#ifndef __FILE_H__
#define __FILE_H__

#include "util.h"

struct File {
    int fildes;
    char *buf;
    u64 size;
};

int open_file(File *file, const char *name);

#endif // __FILE_H__
