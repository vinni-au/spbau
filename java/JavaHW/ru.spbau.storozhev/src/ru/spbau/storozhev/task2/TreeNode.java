package ru.spbau.storozhev.task2;

import java.util.*;

/**
 * Represents node of a tree with text data
 *
 * @author Anton Storozhev
 */
public class TreeNode implements Comparable<TreeNode> {
    /**
     * Constructs a TreeNode with given string
     * @param text String
     */
    public TreeNode(String text) {
        nodeText = text;
    }

    /**
     * Returns list of child nodes
     * @return list of TreeNodes
     */
    public List<TreeNode> getChildNodes() {
        return Collections.unmodifiableList(childNodes);
    }

    /**
     * Returns text associated with TreeNode
     * @return String associated with TreeNode
     */
    public String getNodeText() {
        return nodeText;
    }

    /**
     * Sets a text
     * @param text a String to set
     */
    public void setNodeText(String text) {
        nodeText = text;
    }

    /**
     * Add a new child TreeNode
     * @param child a TreeNode to add
     */
    public void addChild(TreeNode child) {
        childNodes.add(child);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(TreeNode o) {
        return nodeText.compareTo(o.getNodeText());
    }

    /**
     * Sorts child nodes
     */
    public void sortChildNodes() {
        Collections.sort(childNodes);
    }

    private String nodeText;
    private List<TreeNode> childNodes = new ArrayList<>();

}
