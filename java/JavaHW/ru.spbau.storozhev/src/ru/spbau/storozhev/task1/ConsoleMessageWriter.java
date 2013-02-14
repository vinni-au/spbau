package ru.spbau.storozhev.task1;

/**
 * Created with IntelliJ IDEA.
 * User: Anton
 */
public class ConsoleMessageWriter implements MessageWriter {
    public ConsoleMessageWriter() {
    }

    @Override
    public void writeMessage(Message msg) {
        System.out.println("Message " + ++currentMessageNumber);
        for (int i = 0; i < msg.getLines().size(); ++i) {
            System.out.println(currentMessageNumber + "." + (i+1) + ". " + msg.getLines().get(i));
        }
    }

    private int currentMessageNumber = 0;
}
