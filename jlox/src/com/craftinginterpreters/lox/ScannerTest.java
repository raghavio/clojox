package com.craftinginterpreters.lox;

import static com.craftinginterpreters.lox.TokenType.*;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

public class ScannerTest {
    @Test
    public void testScanTokens() {
        Scanner scanner = new Scanner("!=;");
        List<Token> tokens = scanner.scanTokens();

        List<Token> tokensToMatch = new ArrayList<>();
        tokensToMatch.add(new Token(BANG_EQUAL, "!=", null, 1));
        tokensToMatch.add(new Token(SEMICOLON, ";", null, 1));
        tokensToMatch.add(new Token(EOF, "", null, 1));

        assertEquals("Scan !=", tokens, tokensToMatch);
    }

    @Test
    public void testScanTokens2() {
        Scanner scanner = new Scanner("=;");
        List<Token> tokens = scanner.scanTokens();

        List<Token> tokensToMatch = new ArrayList<>();
        tokensToMatch.add(new Token(EQUAL, "=", null, 1));
        tokensToMatch.add(new Token(SEMICOLON, ";", null, 1));
        tokensToMatch.add(new Token(EOF, "", null, 1));

        assertEquals("Scan =", tokens, tokensToMatch);
    }

    @Test
    public void testScanTokensIgnoreComments() {
        Scanner scanner = new Scanner("=; // yoo all of this should get ignored!\n>=;");
        List<Token> tokens = scanner.scanTokens();

        List<Token> tokensToMatch = new ArrayList<>();
        tokensToMatch.add(new Token(EQUAL, "=", null, 1));
        tokensToMatch.add(new Token(SEMICOLON, ";", null, 1));
        tokensToMatch.add(new Token(GREATER_EQUAL, ">=", null, 2));
        tokensToMatch.add(new Token(SEMICOLON, ";", null, 2));
        tokensToMatch.add(new Token(EOF, "", null, 2));

        assertEquals("Scan =; // yoo all of this should get ignored!\n>=;", tokens, tokensToMatch);
    }

    @Test
    public void testScanTokensStrings() {
        Scanner scanner = new Scanner("\"Yooo!\";");
        List<Token> tokens = scanner.scanTokens();

        List<Token> tokensToMatch = new ArrayList<>();
        tokensToMatch.add(new Token(STRING, "\"Yooo!\"", "Yooo!", 1));
        tokensToMatch.add(new Token(SEMICOLON, ";", null, 1));
        tokensToMatch.add(new Token(EOF, "", null, 1));

        assertEquals("Scan \"Yooo!\";", tokens, tokensToMatch);
    }

    @Test
    public void testScanTokensNumbers() {
        Scanner scanner = new Scanner("2342;");
        List<Token> tokens = scanner.scanTokens();

        List<Token> tokensToMatch = new ArrayList<>();
        tokensToMatch.add(new Token(NUMBER, "2342", 2342.0, 1));
        tokensToMatch.add(new Token(SEMICOLON, ";", null, 1));
        tokensToMatch.add(new Token(EOF, "", null, 1));

        assertEquals("Scan 2342;", tokens, tokensToMatch);
    }


    @Test
    public void testScanTokensFractionalNumbers() {
        Scanner scanner = new Scanner("2342.43;");
        List<Token> tokens = scanner.scanTokens();

        List<Token> tokensToMatch = new ArrayList<>();
        tokensToMatch.add(new Token(NUMBER, "2342.43", 2342.43, 1));
        tokensToMatch.add(new Token(SEMICOLON, ";", null, 1));
        tokensToMatch.add(new Token(EOF, "", null, 1));

        assertEquals("Scan 2342.43;", tokens, tokensToMatch);
    }

    @Test
    public void testScanTokensInvalidFractionalNumbers() {
        Scanner scanner = new Scanner("2342.;");
        List<Token> tokens = scanner.scanTokens();

        List<Token> tokensToMatch = new ArrayList<>();
        tokensToMatch.add(new Token(NUMBER, "2342", 2342.0, 1));
        tokensToMatch.add(new Token(DOT, ".", null, 1));
        tokensToMatch.add(new Token(SEMICOLON, ";", null, 1));
        tokensToMatch.add(new Token(EOF, "", null, 1));

        assertEquals("Scan 2342.;", tokens, tokensToMatch);
    }

    @Test
    public void testScanTokensIdentifiers() {
        Scanner scanner = new Scanner("var test = \"Hello world\";");
        List<Token> tokens = scanner.scanTokens();

        List<Token> tokensToMatch = new ArrayList<>();
        tokensToMatch.add(new Token(VAR, "var", null, 1));
        tokensToMatch.add(new Token(IDENTIFIER, "test", null, 1));
        tokensToMatch.add(new Token(EQUAL, "=", null, 1));
        tokensToMatch.add(new Token(STRING, "\"Hello world\"", "Hello world", 1));
        tokensToMatch.add(new Token(SEMICOLON, ";", null, 1));
        tokensToMatch.add(new Token(EOF, "", null, 1));

        assertEquals("Scan var test = \"Hello world\";", tokens, tokensToMatch);
    }
}
