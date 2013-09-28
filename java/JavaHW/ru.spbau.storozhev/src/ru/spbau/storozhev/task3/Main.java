package ru.spbau.storozhev.task3;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.*;

/**
 * Main class for hw3
 *
 * @author Anton Storozhev
 */
public class Main {
    /**
     * Entry point for the program
     *
     * Usage:
     *  compress <output file> <list of files or dirs>
     *  decompress <input file> [<output dir>]
     * @param args program arguments
     *
     */
    public static void main(String[] args) {
        if (args.length < 2) {
            System.err.println("Usage: compress <output> <file|dir> ... \n decompress <file> [<outputdir>]");
            return;
        }

        if (args[0].equals("compress")) {
            if (args.length < 3) {
                System.err.println("You need to specify input files (paths) to compress");
                return;
            }

            List<String> paths = new ArrayList<String>();
            for (int i = 2; i < args.length; ++i) {
                paths.add(args[i]);
            }

            Archiver.compress(args[1], paths);
        } else

        if (args[0].equals("decompress")) {
            if (args.length >= 3) {
                Archiver.decompress(args[1], args[2]);
            } else {
                Archiver.decompress(args[1]);
            }
        } else {
            System.err.println("Seems to be unknown command. Try \"compress\" or \"decompress\" ");
        }

    }
}
