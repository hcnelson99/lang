#ifndef __TOKENIZER_H__
#define __TOKENIZER_H__

#include "file.h"
#include "vec.h"
#include "util.h"

enum class TokType {
    Eof,
    Ident,
    IntLit,
    Plus = '+',
    Minus = '-',
    Times = '*',
    Divide = '/',
};

struct Token {
    TokType type;
    char *ident;
    u64 int_lit;
};

struct Tokenizer {
    Vec<Token> tokens;
    u64 pos;


    static Tokenizer create(File *f);

    void tokenize();

private:
    void finish_number();

    enum State {
        Begin,
        InNumber,
    } state;

    File *f;
    Token tok;
    u64 i;
};

void print_tok(Token *tok);

#endif // __TOKENIZER_H__
