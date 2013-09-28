package ru.spbau.storozhev.task2;

import java.io.FileNotFoundException;

/**
 * Main class
 *
 * @author Anton Storzhev
 */
public class Main {
    /**
     * Program entry point
     * Arguments: absolute_path_to_dir
     * @param args command line arguments
     */
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: <absolute path to dir>");
            return;
        }

        FilesystemWalker walker = new FilesystemWalker(args[0]);
        walker.printTree();
    }
}
