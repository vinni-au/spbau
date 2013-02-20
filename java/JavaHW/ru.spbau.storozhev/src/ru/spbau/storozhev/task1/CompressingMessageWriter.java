package ru.spbau.storozhev.task1;

/**
 * Writes messages: consequent pairs of messages will be concatenated in one message.
 * Uses another MessageWriter to write messages
 *
 * @author Anton Storozhev
 */
public class CompressingMessageWriter implements MessageWriter {
    /**
     * Constructs CompressingMessageWriter with given MessageWriter
     * @param mw a MessageWriter to use
     */
    public CompressingMessageWriter(MessageWriter mw) {
        messageWriter = mw;
    }

    /**
     * Writes two consequent messages as one message.
     * Thereby two call needed to message actually be written
     * @param msg a Message to write
     */
    @Override
    public void writeMessage(Message msg) {
        if (message == null) {
            message = msg;
        } else {
            message.append(msg);
            messageWriter.writeMessage(message);
            message = null;
        }
    }

    /**
     * Closes writer
     * @throws RuntimeException when MessageWriter can't be close
     */
    @Override
    public void close() throws RuntimeException {
        if (messageWriter != null) {
            if (message != null) {
                messageWriter.writeMessage(message);
                message = null;
            }
            messageWriter.close();
        }
    }

    private MessageWriter messageWriter = null;
    private Message message = null;
}
