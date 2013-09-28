package ru.spbau.storozhev.task1;

/**
 * Writes Messages to console
 *
 * @author Anton Storozhev
 */
public class ConsoleMessageWriter implements MessageWriter {
    /**
     * Constructs a ConsoleMessageWriter
     */
    public ConsoleMessageWriter() {
    }

    /**
     * Prints given message to console in the following format:
     * "Message #number of message"
     * "#messagenumber.#linenumber line of message"
     * @param msg Message to print
     */
    @Override
    public void writeMessage(Message msg) {
        System.out.println("\"Message " + ++currentMessageNumber + "\"");
        for (int i = 0; i < msg.getLines().size(); ++i) {
            System.out.println("\"" + currentMessageNumber + "." + (i+1) + ". \"" + msg.getLines().get(i));
        }
    }

    /**
     * Closes the Writer (actually does nothing)
     */
    @Override
    public void close() {
        //there's nothing to do
    }

    private int currentMessageNumber = 0;
}
