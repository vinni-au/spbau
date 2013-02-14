/**
 * Created with IntelliJ IDEA.
 * User: Anton
 */
package ru.spbau.storozhev.task1;

public class Main {
    public static void main(String[] args) {
        String filename = args[0];
        FileMessageReader fileMessageReader = new FileMessageReader(filename);
        ConsoleMessageWriter consoleMessageWriter = new ConsoleMessageWriter();
        FileMessageWriter fileMessageWriter = new FileMessageWriter("output.txt");
        try {
            fileMessageWriter.writeMessage(fileMessageReader.readMessage());
            fileMessageWriter.writeMessage(fileMessageReader.readMessage());
        } catch (IllegalMessageFormatException e) {

        }
    }
}
