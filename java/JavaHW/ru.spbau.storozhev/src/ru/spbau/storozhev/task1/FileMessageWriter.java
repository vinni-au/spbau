/**
 * Created with IntelliJ IDEA.
 * User: Anton
 */
package ru.spbau.storozhev.task1;
import java.io.*;
import java.nio.file.*;

public class FileMessageWriter implements MessageWriter {
    public FileMessageWriter(String fname) {
        filename = fname;
        File f = new File(filename);
        if (!f.exists()) {
            //create file
        }
    }

    @Override
    public void writeMessage(Message msg) {
        try {
            PrintWriter printWriter = new PrintWriter(new File(filename));
            printWriter.println(msg.getLines().size());
            for (int i = 0; i < msg.getLines().size(); ++i)
                printWriter.println(msg.getLines().get(i));
            printWriter.close();
        }  catch (FileNotFoundException e) {
            System.err.println("File not found: " + filename);
        }
    }

    private String filename;
}
