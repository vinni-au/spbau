package ru.spbau.storozhev.task2;

import java.io.File;
import java.io.FileNotFoundException;
import java.lang.StringBuilder;
import java.security.AccessControlException;
import java.util.regex.Pattern;

/**
 * Walks through file system and prints directory tree
 *
 * @author Anton Storozhev
 */
public class FilesystemWalker {
    /**
     * Constructs a FilesystemWalker with given path
     * @param path_ path to directory (or file)
     * @throws FileNotFoundException if dir or file wasn't found
     */
    public FilesystemWalker(String path_) throws FileNotFoundException {
        path = path_;
        if (!(new File(path_)).exists())
            throw new FileNotFoundException();
        pattern = Pattern.compile("\\..*");
    }

    /**
     * Construct a FilesystemWalker with given path and filter (files and dirs, which matches to the filter, shall not be printed)
     * @param path path do directory (or file)
     * @param filter filter
     * @throws FileNotFoundException if dir or file wasn't found
     */
    public FilesystemWalker(String path, String filter) throws FileNotFoundException {
        this(path);
        pattern = Pattern.compile("((\\..*)|.*(" + filter + ").*)");
    }

    /**
     * Prints a filesystem tree to the console
     */
    public void printTree() {
        createTree();

        System.out.println(rootNode.getNodeText());
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < rootNode.getNodeText().length(); ++i) {
            sb.append(' ');
        }
        for (int i = 0; i < rootNode.getChildNodes().size(); ++i) {
            printTreeHelper(rootNode.getChildNodes().get(i), sb.toString(), i < rootNode.getChildNodes().size() - 1);
        }
    }

    /**
     * Recursive method to print subtree
     * @param node node to print
     * @param leftspace left placeholder of a tree
     * @param midNeeded indicates whether `|` symbol is needed
     */
    private void printTreeHelper(TreeNode node, String leftspace, boolean midNeeded) {
        System.out.println(leftspace + "|_" + node.getNodeText());
        StringBuilder sb = new StringBuilder(leftspace);
        if (midNeeded) {
            sb.append("|");
        } else {
            sb.append(" ");
        }
        for (int i = 0; i <= node.getNodeText().length(); ++i) {
            sb.append(" ");
        }
        for (int i = 0; i < node.getChildNodes().size(); ++i) {
            printTreeHelper(node.getChildNodes().get(i), sb.toString(), i < node.getChildNodes().size() - 1);
        }
    }

    /**
     * Creates a tree to represent filesystem
     */
    private void createTree() {
        try {
            rootDir = new File(path);
            rootNode = new TreeNode(rootDir.getName());
            if (rootDir.isDirectory()) {
                createTreeHelper(rootNode, rootDir);
                checkDirPermissions(rootNode, rootDir);
            } else {
                checkFilePermissions(rootNode, rootDir);
            }
        }  catch (AccessControlException e) {
            rootNode.setNodeText(rootNode.getNodeText() + " (access denied)");
        }
    }


    /**
     * Helper method to create subtree
     * @param node current node
     * @param dir directory to scan
     */
    private void createTreeHelper(TreeNode node, File dir) {
        if (dir.listFiles() != null) {
            for (File file : dir.listFiles()) {
                if (!pattern.matcher(file.getName()).matches()) {
                    TreeNode child = new TreeNode(file.getName());
                    node.addChild(child);
                    try {
                        if (file.isDirectory()) {
                            createTreeHelper(child, file);
                            checkDirPermissions(child, file);
                        } else {
                            checkFilePermissions(child, file);
                        }
                    } catch (AccessControlException e) {
                        child.setNodeText(child.getNodeText() + " (access denied)");
                    }
                }
            }
        }
    }

    /**
     * Check dir permissions and adds " (access denied)" to node text if subdirs of a dir can't be gotten
     * @param node node
     * @param dir dir to check
     */
    private void checkDirPermissions(TreeNode node, File dir) {
        if (!dir.canExecute()) {
            node.setNodeText(node.getNodeText() + " (access denied)");
        }
    }

    /**
     * Check file permissions and adds " (access denied)" to node text if file can't be read
     * @param node node
     * @param file file to check
     */
    private void checkFilePermissions(TreeNode node, File file) {
        if (!file.canRead()) {
            node.setNodeText(node.getNodeText() + " (access denied)");
        }
    }

    private File rootDir;
    private String path;
    private TreeNode rootNode;
    private Pattern pattern;
}
