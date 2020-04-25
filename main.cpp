#include <stdio.h>

#include "tokenizer.h"
#include "util.h"
#include "vec.h"
#include "file.h"

int main(void) {
    File f;
    int ret = open_file(&f, "source");
    Tokenizer t = Tokenizer::create(&f);
    t.tokenize();
    ASSERT(ret == 0);

    For (t.tokens) {
        print_tok(it);
        printf(" ");
    }
    printf("\n");
}
