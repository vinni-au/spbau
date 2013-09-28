package ru.spbau.storozhev.task1;
import java.io.*;

/**
 * Writes Messages to file
 *
 * @author Anton Storozhev
 */
public class FileMessageWriter implements MessageWriter {
    /**
     * Constructs FileMessageWriter with given file name
     * @param filename filename to write Messages to
     * @throws FileNotFoundException if file wasn't found
     */
    public FileMessageWriter(String filename) throws FileNotFoundException {
        printWriter = new PrintWriter(new File(filename));
    }

    /**
     * Writes given message to file
     * @param msg a Message to write
     */
    @Override
    public void writeMessage(Message msg) {
        printWriter.println(msg.getLines().size());
        for (int i = 0; i < msg.getLines().size(); ++i)
            printWriter.println(msg.getLines().get(i));
    }

    /**
     * Closes the writer
     */
    @Override
    public void close() {
        if (printWriter != null)
            printWriter.close();
    }

    private String filename;
    private PrintWriter printWriter = null;
}
