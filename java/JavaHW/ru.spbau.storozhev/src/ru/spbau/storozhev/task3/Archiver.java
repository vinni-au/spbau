package ru.spbau.storozhev.task3;


import java.util.*;
import java.io.*;
/**
 * Utility class for compression/decompression
 *
 * @author Anton Storozhev
 */
public class Archiver {
    public static void compress(String filename, List<String> paths) {
        File bundle = Archiver.bundle(paths);
    }


    public static void decompress(String filename, String outputPath) {

    }

    public static void decompress(String filename) {
        decompress(filename, System.getProperty("user.dir"));
    }

    private static File bundle(List<String> paths) {
        return new File("");
    }

    private static void unbundle(File file, String outputPath) {

    }
}
