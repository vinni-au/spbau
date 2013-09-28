package ru.spbau.storozhev.task3;

import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.util.*;
import java.io.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * Utility class for compression/decompression
 *
 * @author Anton Storozhev
 */
public class Archiver {

    /**
     * Compresses all files in paths recursively into file by given filename
     * @param filename filename for archive
     * @param paths input paths
     */
    public static void compress(String filename, List<String> paths) {
        File bundle = null;
        try {
            bundle = bundle(paths);
        } catch (IOException e) {
            System.err.println("IOException during creating bundle");
            return;
        }
        File outputFile = new File(filename);
        if (!outputFile.exists()) {
            try {
                outputFile.createNewFile();
            } catch (IOException e) {
                System.out.println("Can't create output file");
            }
        }

        ZipOutputStream zipStream = null;
        FileInputStream inputStream = null;
        try {
            zipStream = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(outputFile)));
            inputStream = new FileInputStream(bundle);

            zipStream.putNextEntry(new ZipEntry(bundle.getName()));

            int length;
            byte[] buffer = new byte[2048];
            while ((length = inputStream.read(buffer)) > 0) {
                zipStream.write(buffer, 0, length);
            }
        } catch (FileNotFoundException e) {
            System.err.println("File not found during compress operation");
        } catch (IOException e) {
            System.err.println("IOException during compress operation");
        } finally {
            try {
                if (inputStream != null)
                    inputStream.close();
            } catch (IOException e) {
                System.err.println("IOException during closing InputStream");
            }

            try {
                if (zipStream != null)
                    zipStream.close();
            } catch (IOException e) {
                System.err.println("IOException during closing ZipStream");
            }

            bundle.delete();
        }
    }

    /**
     * Decompresses archive by given filename and output path
     * @param filename filename of an archive
     * @param outputPath path to decompress
     */
    public static void decompress(String filename, String outputPath) {
        ZipInputStream zipStream = null;
        try {
            zipStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(filename)));
        } catch (FileNotFoundException e) {
            System.out.println("Can't find file to decompress");
            return;
        }
        ZipEntry entry = null;
        try {
            entry = zipStream.getNextEntry();
        } catch (IOException e) {
            System.out.println("IOException during reading zip file. Is it really a zip file?");
            return;
        }

        File bundle = new File(entry.getName());
        BufferedOutputStream output = null;
        try {
            if (!bundle.exists())
                bundle.createNewFile();
            output = new BufferedOutputStream(new FileOutputStream(bundle));

            int length;
            byte[] buffer = new byte[2048];
            while ((length = zipStream.read(buffer, 0, buffer.length)) != -1) {
                output.write(buffer, 0, length);
            }
        } catch (IOException e) {
            System.err.println("IOException during unzip process");
        } finally {
            try {
                if (output != null)
                    output.close();
            } catch (IOException e) {
                System.err.println("IOException during closing BufferedOutputStream");
            }
            try {
                if (zipStream != null)
                    zipStream.close();
            } catch (IOException e) {
                System.err.println("IOException during closing ZipInputStream");
            }
        }

        try {
            unbundle(new File(entry.getName()), outputPath);
        } catch (FileNotFoundException e) {
            System.out.println("FileNotFound exception");
        }

        bundle.delete();
    }

    /**
     * Decompresses archive by given filename into current dir
     * @param filename filename of an archive
     */
    public static void decompress(String filename) {
        decompress(filename, System.getProperty("user.dir"));
    }

    /**
     * Creates a bundle from all files in given paths
     * @param paths paths with files to bundle
     * @return file represents bundle
     * @throws IOException in case of IOException during bundling
     */
    private static File bundle(List<String> paths) throws IOException {
        System.out.println("Scanning paths...");
        List<File> files = new ArrayList<>();
        Queue<File> dirs = new ArrayDeque<>();
        for (String path : paths) {
            File file = new File(path);
            if (file.exists() && file.canRead()) {
                if (file.isDirectory()) {
                    dirs.add(file);
                } else {
                    files.add(file);
                }
            }
        }
        while (!dirs.isEmpty()) {
            File dir = dirs.poll();
            if (dir.canRead()) {
                for (File file : dir.listFiles()) {
                    if (file.canRead()) {
                        if (file.isDirectory())
                            dirs.add(file);
                        else
                            files.add(file);
                    } else {
                        System.out.println("Unable to read file: " + file.getPath());
                    }
                }
            } else {
                System.out.println("Unable to read directory: " + dir.getPath());
            }
        }

        File result = File.createTempFile("bundle-", null);
        if (!result.exists())
            result.createNewFile();

        OutputStream outputStream = new FileOutputStream(result);

        byte[] buffer = new byte[2048];

        for (File file : files) {
            System.out.println("Adding to bundle: " + file.getName());
            InputStream input = null;
            try {
                long length = file.length();
                String path = file.getPath();
                input = new FileInputStream(file);

                byte[] pathb = path.getBytes("UTF-8");
                outputStream.write(ByteBuffer.allocate(4).putInt(pathb.length).array());
                outputStream.write(pathb);
                outputStream.write(ByteBuffer.allocate(8).putLong(length).array());
                int l;
                while ((l = input.read(buffer)) != -1) {
                    outputStream.write(buffer, 0, l);
                }
            } catch (IOException e) {
                System.err.println("IOException during reading file " + file.getName());
            } finally {
                try {
                    input.close();
                } catch (IOException e) {
                    System.err.println("IOException during closing file" + file.getName());
                }
            }
        }

        outputStream.close();

        return result;
    }

    /**
     * Unbundles given bundle to files
     * @param bundle bundle to unbundle
     * @param outputPath path to put files
     * @throws FileNotFoundException in case of bundle file wasn't found
     */
    private static void unbundle(File bundle, String outputPath) throws FileNotFoundException {
        if (!outputPath.endsWith(File.separator)) {
            outputPath = new String(outputPath + File.separator);
        }
        FileInputStream inputStream = new FileInputStream(bundle);
        FileChannel inChannel = inputStream.getChannel();

        byte[] buffer = new byte[2048];
        try {
            ByteBuffer readBuffer = ByteBuffer.allocate(16 * 1024);
            while (inputStream.available() > 0) {
                ByteBuffer bStrLen = ByteBuffer.allocate(4);
                if (inChannel.read(bStrLen) == -1)
                    break;
                bStrLen.flip();
                int strLen = bStrLen.getInt();
                ByteBuffer bStr = ByteBuffer.allocate(strLen);
                if (inChannel.read(bStr) == -1)
                    break;
                bStr.flip();
                String path = new String(bStr.array(), "UTF-8");
                System.out.println("Unbundling file: " + path);
                File file = new File(outputPath + path);
                if (!file.exists()) {
                    file.getParentFile().mkdirs();
                    file.createNewFile();
                }
                OutputStream output = new FileOutputStream(file);
                ByteBuffer bLen = ByteBuffer.allocate(8);
                if (inChannel.read(bLen) == -1)
                    break;
                bLen.flip();

                long fileLen = bLen.getLong();
                while (fileLen > 0) {
                    int read = inputStream.read(buffer, 0, fileLen > 2048 ? 2048 : (int)fileLen);
                    fileLen -= 2048;
                    output.write(buffer, 0, read);
                }
                output.close();
            }
        } catch (IOException e) {
            System.err.println("IOException while unbundling");
        } catch (NumberFormatException e) {
            System.out.println("Illegal number format");
        }
    }

}
