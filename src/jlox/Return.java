package jlox;

public class Return extends RuntimeException {
    public final Object value;

    public Return(Object value) {
        super(null, null, false, false); // Disable stacktrace.
        this.value = value;
    }
}
