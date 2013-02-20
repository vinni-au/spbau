package ru.spbau.storozhev.task1;

import java.util.*;

/**
 * A message of multiple lines
 *
 * @author Anton Storozhev
 */
public class Message {
    /**
     * Constructs an empty message
     */
    public Message() {
        lines = new ArrayList<String>();
    }

    /**
     * Constructs a message
     * @param strings strings of message
     */
    public Message(List<String> strings) {
        lines = strings;
    }

    /**
     * Appends lines of other Message to current Message
     * @param msg Message to append
     */
    public void append(Message msg) {
        lines.addAll(msg.lines);
    }

    /**
     * Returns lines of current message
     * @return list of lines
     */
    public List<String> getLines() {
        return Collections.unmodifiableList(lines);
    }

    private List<String> lines;
}
