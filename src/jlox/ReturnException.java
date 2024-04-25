package jlox;

public class ReturnException extends RuntimeException {
    public final Object value;

    public ReturnException(Object value) {
        super(null, null, false, false); // Disable stacktrace.
        this.value = value;
    }
}
