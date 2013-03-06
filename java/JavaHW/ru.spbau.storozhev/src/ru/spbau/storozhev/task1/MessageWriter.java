package ru.spbau.storozhev.task1;

import java.io.IOException;

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
    void writeMessage(Message msg);


    /**
     * Closes the MessageWriter (if needed)
     * @throws IOException when writer can't be closed
     */
    void close() throws IOException;
}
