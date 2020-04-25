#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "util.h"
#include "file.h"

int open_file(File *file, const char *name) {
    ASSERT(file);
    file->fildes = open(name, O_RDONLY);
    if (file->fildes < 0) {
        perror("source");
        return -1;
    }
    struct stat metadata;
    if (fstat(file->fildes, &metadata) < 0) {
        return -1;
    }
    file->size = metadata.st_size;

    file->buf = (char*)mmap(NULL, file->size, PROT_READ, MAP_PRIVATE, file->fildes, 0);
    if (file->buf == MAP_FAILED) {
        perror(name);
        return -1;
    }
    return 0;
}



