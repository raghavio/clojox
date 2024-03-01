package jlox;

import static java.lang.StringTemplate.STR;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

public class Lox {
    public static boolean hadError = false;
    public static boolean hadRuntimeError = false;

    public static void runREPL() throws IOException {
        InputStreamReader input = new InputStreamReader(System.in);
        BufferedReader reader = new BufferedReader(input);

        for (;;) {
            System.out.print("> ");
            String line = reader.readLine(); // Read
            if (line == null)
                break;
            run(line); // Eval
            hadError = false; // Reset the error flag for next input
        } // Loop
    }

    public static void run(String source) {
        Scanner scanner = new Scanner(source);
        List<Token> tokens = scanner.scanTokens();

        // For now, just print the tokens.
        for (Token token : tokens) {
            System.out.println(token);
        }
    }

    public static void error(int line, String message) {
        report(line, "", message);
    }

    public static void report(int line, String where, String message) {
        System.err.println(STR."[line \{line}] Error\{where}: \{message}");
        hadError = true;
    }

    public static void runtimeError(Token token, String message) {
        if (token.type() == TokenType.EOF) {
            report(token.line(), " at end", message);
        } else {
            report(token.line(), " at '" + token.lexeme() + "'", message);
        }
    }
}
