package ru.spbau.storozhev.task1;

/**
 * Created with IntelliJ IDEA.
 * User: Anton
 */
public class CompressingMessageWriter implements MessageWriter {
    public CompressingMessageWriter(MessageWriter mw) {
        messageWriter = mw;
    }

    @Override
    public void writeMessage(Message msg) {

    }

    private MessageWriter messageWriter;
}
