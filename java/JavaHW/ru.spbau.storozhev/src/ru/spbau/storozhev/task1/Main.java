package ru.spbau.storozhev.task1;

import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * Main class
 *
 * @author Anton Storozhev
 */
public class Main {
    /**
     * Entry point
     * Arguments: input_file [output_file]
     * @param args command line arguments
     */
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Parameters is: <input file> [<output file>]");
            return;
        }

        FileMessageReader fileMessageReader = null;
        String filename = "";
        try {
            filename = args[0];
            fileMessageReader = new FileMessageReader(filename);
        } catch (FileNotFoundException e) {
            System.err.println("File \"" + filename + "\" wasn't found");
            if (fileMessageReader != null) {
                try {
                    fileMessageReader.close();
                } catch (IOException exc) {
                    System.err.println("Unexpected IOException occurred");
                }
            }
        }

        MessageWriter messageWriter = null;
        try {
            if (args.length > 1) {
                String outputFilename = args[1];

                messageWriter = new CompressingMessageWriter(new FileMessageWriter(outputFilename));
            } else {
                messageWriter = new CompressingMessageWriter(new ConsoleMessageWriter());
            }
        } catch (FileNotFoundException e) {
            System.err.println("Output file wasn't found");
        }

        try {
            Message message = fileMessageReader.readMessage();
            while (message != null) {
                messageWriter.writeMessage(message);
                message = fileMessageReader.readMessage();
            }
        } catch (IllegalMessageFormatException e) {
            System.err.println("Message has inappropriate format");
        } catch (IOException e) {
            System.err.println("IOException occurred");
        }

        try {
            messageWriter.close();
        } catch (RuntimeException e) {
            System.err.println("Error: can't close MessageWriter");
        }
    }

}
