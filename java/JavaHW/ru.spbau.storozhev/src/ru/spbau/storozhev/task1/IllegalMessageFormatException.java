package ru.spbau.storozhev.task1;

/**
 * This exception thrown when message format is invalid
 *
 * @author Anton Storozhev
 */
public class IllegalMessageFormatException extends Exception {
    /**
     * Constructs IllegalMessageFormatException with given message
     * @param msg exception informational message
     */
    public IllegalMessageFormatException(String msg) {
        super(msg);
    }
}
