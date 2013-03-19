package ru.spbau.storozhev.task3;

import java.util.*;

/**
 * Main class for hw3
 *
 * @author Anton Storozhev
 */
public class Main {
    public static void main(String[] args) {
        if (args.length < 2) {
            System.err.println("Usage: compress <output> <file|dir> ... / decompress <file> [<outputdir>]");
            return;
        }

        if (args[0] == "compress") {
            if (args.length < 3) {
                System.err.println("You need to specify input files (paths) to compress");
                return;
            }

            List<String> paths = new ArrayList<String>();
            for (int i = 2; i < args.length; ++i) {
                paths.add(args[i]);
            }

            Archiver.compress(args[1], paths);
        }

        if (args[0] == "decompress") {
            if (args.length >= 3) {
                Archiver.decompress(args[1], args[2]);
            } else {
                Archiver.decompress(args[1]);
            }
        }

        System.err.println("Seems to be unknown command. Try \"compress\" or \"decompress\" ");

    }
}
