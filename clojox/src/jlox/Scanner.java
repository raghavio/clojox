package jlox;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static jlox.TokenType.*;

public class Scanner {
    private final String source;
    private final List<Token> tokens = new ArrayList<>();
    private int start = 0;
    private int current = 0;
    private int line = 1;
    private static final Map<String, TokenType> keywords = Map.ofEntries(
            new SimpleEntry<>("and", TokenType.AND),
            new SimpleEntry<>("class", TokenType.CLASS),
            new SimpleEntry<>("else", TokenType.ELSE),
            new SimpleEntry<>("false", TokenType.FALSE),
            new SimpleEntry<>("for", TokenType.FOR),
            new SimpleEntry<>("fun", TokenType.FUN),
            new SimpleEntry<>("if", TokenType.IF),
            new SimpleEntry<>("nil", TokenType.NIL),
            new SimpleEntry<>("or", TokenType.OR),
            new SimpleEntry<>("print", TokenType.PRINT),
            new SimpleEntry<>("return", TokenType.RETURN),
            new SimpleEntry<>("super", TokenType.SUPER),
            new SimpleEntry<>("this", TokenType.THIS),
            new SimpleEntry<>("true", TokenType.TRUE),
            new SimpleEntry<>("var", TokenType.VAR),
            new SimpleEntry<>("while", TokenType.WHILE));

    public Scanner(String source) {
        this.source = source;
    }

    public List<Token> scanTokens() {
        while (hasCharacters()) {
            // We are at the beginning of the next lexeme.
            start = current;
            scanToken();
        }

        tokens.add(new Token(EOF, "", null, line));
        return tokens;
    }

    /**
     * Checks if the source has any characters left to scan or not
     * 
     * @return
     */
    private boolean hasCharacters() {
        return source.length() > current;
    }

    private void scanToken() {
        char c = getAndAdvanceChar();
        switch (c) {
            case '(':
                addToken(LEFT_PAREN);
                break;
            case ')':
                addToken(RIGHT_PAREN);
                break;
            case '{':
                addToken(LEFT_BRACE);
                break;
            case '}':
                addToken(RIGHT_BRACE);
                break;
            case ',':
                addToken(COMMA);
                break;
            case '.':
                addToken(DOT);
                break;
            case '-':
                addToken(MINUS);
                break;
            case '+':
                addToken(PLUS);
                break;
            case ';':
                addToken(SEMICOLON);
                break;
            case '*':
                addToken(STAR);
                break;
            case '!':
                if (isNextChar('=')) {
                    current += 1;
                    addToken(BANG_EQUAL);
                } else {
                    addToken(BANG);
                }
                break;
            case '=':
                if (isNextChar('=')) {
                    current += 1;
                    addToken(EQUAL_EQUAL);
                } else {
                    addToken(EQUAL);
                }
                break;
            case '<':
                if (isNextChar('=')) {
                    current += 1;
                    addToken(LESS_EQUAL);
                } else {
                    addToken(LESS);
                }
                break;
            case '>':
                if (isNextChar('=')) {
                    current += 1;
                    addToken(GREATER_EQUAL);
                } else {
                    addToken(EQUAL);
                }
                break;
            case '/':
                if (isNextChar('/')) {
                    do {
                        current += 1; // Skip till loop ends.
                    } while (hasCharacters() && !isNextChar('\n')); // Loop till next char is a new line.
                } else
                    addToken(SLASH);
                break;
            case '"':
                parseString();
                break;
            case ' ':
            case '\r':
            case '\t':
                // Ignore whitespace.
                break;
            case '\n':
                line += 1;
                break;
            default:
                if (isDigit(c)) {
                    pasreNumber();
                } else if (isAlpha(c)) {
                    identifier();
                } else
                    Lox.error(line, "Unexpected character.");
        }
    }

    private void identifier() {
        while (hasCharacters() && isAlphaNumeric(source.charAt(current)))
            current += 1;
        String text = source.substring(start, current);
        TokenType type = keywords.get(text);
        if (type == null)
            type = IDENTIFIER;
        addToken(type);
    }

    private void pasreNumber() {
        // Keep advancing if characters are digits or period (.) and then digits.
        while (hasCharacters() && (isDigit(source.charAt(current)) || (isNextChar('.') && isDigit(peekNext()))))
            current += 1;

        addToken(NUMBER, Double.parseDouble(source.substring(start, current)));
    }

    private char peekNext() {
        if (current + 1 >= source.length())
            return '\0';
        return source.charAt(current + 1);
    }

    private boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    /**
     * Parse the string from raw source. Increment the line counter if string is
     * multiline.
     */
    private void parseString() {
        while (hasCharacters() && !isNextChar('"')) {
            if (isNextChar('\n'))
                line += 1;
            current += 1;
        }

        if (!hasCharacters()) {
            Lox.error(line, "Unterminated string. Missing '\"'");
            return;
        }

        current += 1; // Skip the closing "
        String value = source.substring(start + 1, current - 1);
        addToken(STRING, value);
    }

    private boolean isAlpha(char c) {
        return (c >= 'a' && c <= 'z') ||
                (c >= 'A' && c <= 'Z') ||
                c == '_';
    }

    private boolean isAlphaNumeric(char c) {
        return isAlpha(c) || isDigit(c);
    }

    /**
     * Gets the current character and advances the counter.
     */
    private char getAndAdvanceChar() {
        return source.charAt(current++);
    }

    private void addToken(TokenType type) {
        addToken(type, null);
    }

    private void addToken(TokenType type, Object literal) {
        String text = source.substring(start, current);
        tokens.add(new Token(type, text, literal, line));
    }

    /**
     * Checks if the next character in the source is the expected character that's
     * passed.
     */
    private boolean isNextChar(char expected) {
        if (!hasCharacters() || source.charAt(current) != expected)
            return false;
        return true;
    }

}
