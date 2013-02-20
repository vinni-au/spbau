package ru.spbau.storozhev.task1;

import java.io.*;
import java.util.*;

/**
 * Reads message from file
 *
 * @author Anton Storozhev
 */
public class FileMessageReader {
    /**
     * Constructs FileMessageReader
     * @param filename name of a file with messages
     */
    public FileMessageReader(String filename) throws  FileNotFoundException{
            br = new BufferedReader(new FileReader(new File(filename)));
    }

    /**
     * Reads next Message from file
     * @return Message which was read, or null if there no more messages in file
     * @throws IllegalMessageFormatException
     * @throws IOException
     */
    public Message readMessage() throws IllegalMessageFormatException, IOException {
        List<String> lines = new ArrayList<String>();
        int numlines = 0;
        try {
            String firstLine = br.readLine();
            if (firstLine == null)
                return null;
            numlines = Integer.parseInt(firstLine);
        } catch (IOException e) {
            throw e;
        } catch (NumberFormatException e) {
            throw new IllegalMessageFormatException("Message has illegal format");
        }
        if (numlines < 0) {
            throw new IllegalMessageFormatException("Message has illegal format: lines number must be positive");
        }
        for (int i = 0; i < numlines; ++i) {
            String line = br.readLine();
            if (line == null)
                throw new IllegalMessageFormatException("Message has illegal format: unexpected end of file");
            lines.add(line);
        }

        return new Message(lines);
    }

    /**
     * Closes the file
     * @throws IOException if BufferedReader can't close the file
     */
    public void close() throws IOException {
        if (br != null)
            br.close();
    }

    private BufferedReader br = null;
}
