package com.craftinginterpreters.lox;

public record Token(TokenType type, String lexeme, Object literal, int line) {

    @Override
    public String toString() {
        return type + " " + lexeme + " " + literal;
    }
}

