package ru.spbau.storozhev.task1;

/**
 * MessageWriter implementers should be able to write Messages
 *
 * @author Anton Storozhev
 */
public interface MessageWriter {
    /**
     * Writes a given message
     * @param msg a Message to write
     */
    public void writeMessage(Message msg);


    /**
     * Closes the MessageWriter (if needed)
     */
    public void close() throws RuntimeException;
}
