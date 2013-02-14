/**
 * Created with IntelliJ IDEA.
 * User: Anton
 */
package ru.spbau.storozhev.task1;

import java.io.*;
import java.util.*;

public class FileMessageReader {
    public FileMessageReader(String filename) {
        try {
            br = new BufferedReader(new FileReader(new File(filename)));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public Message readMessage() throws IllegalMessageFormatException {
        List<String> lines = new ArrayList<String>();
        try {
            String firstLine = br.readLine();
            int numlines = Integer.parseInt(firstLine);
            for (int i = 0; i < numlines; ++i)
                lines.add(br.readLine());
        } catch (IOException e) {
            e.printStackTrace();
        } catch (NumberFormatException e) {
            throw new IllegalMessageFormatException();
        }
        return new Message(lines);
    }

    private BufferedReader br;
}
