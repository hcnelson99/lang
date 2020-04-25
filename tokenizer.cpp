#include "tokenizer.h"

void print_tok(Token *tok) {
    switch(tok->type) {
        case TokType::Eof:
            printf("Eof");
            break;
        case TokType::Ident:
            printf("Ident(%s)", tok->ident);
            break;
        case TokType::IntLit:
            printf("Int(%lu)", tok->int_lit);
            break;
        case TokType::Plus:
            printf("Plus");
            break;
        case TokType::Times:
            printf("Times");
            break;
        case TokType::Minus:
            printf("Minus");
            break;
        case TokType::Divide:
            printf("Divide");
            break;
    }
}

Token peek(Tokenizer *t) {
    return t->tokens[t->pos];
}

Token pop(Tokenizer *t) {
    return t->tokens[t->pos++];
}

void Tokenizer::finish_number() {
    tokens.add(tok);
    state = Begin;
}

Tokenizer Tokenizer::create(File *f) {
    Tokenizer tokenizer;
    tokenizer.tokens = Vec<Token>::empty();
    tokenizer.pos = 0;
    tokenizer.f = f;
    return tokenizer;
}

void Tokenizer::tokenize() {
    i = 0;
    state = Begin;
    while (1) {
        if (i == f->size) {
            tok.type = TokType::Eof;
            tokens.add(tok);
            return;
        }
        char c = f->buf[i];
        switch (c) {
            case '+': 
            case '-': 
            case '*': 
            case '/': 
                switch(state) {
                case InNumber:
                    finish_number();
                    break;
                case Begin:
                    tok.type = static_cast<TokType>(c);
                    tokens.add(tok);
                }
                break;
            case '0'...'9':
                switch (state) {
                case Begin:
                    tok.type = TokType::IntLit;
                    tok.int_lit = c - '0';
                    state = InNumber;
                    break;
                case InNumber:
                    tok.int_lit *= 10;
                    tok.int_lit += c - '0';
                    break;
                default:
                    ASSERT(false);
                }
                break;
            case '\t':
                // TODO: error message
                ASSERT(false);
            case ' ':
            case '\n':
                switch (state) {
                case Begin:
                    break;
                case InNumber:
                    finish_number();
                    break;
                default:
                    ASSERT(false);
                }
                break;
            default:
                ASSERT(false);
        }
        i += 1;
    }
}
