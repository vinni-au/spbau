/**
 * Created with IntelliJ IDEA.
 * User: Anton
 */
package ru.spbau.storozhev.task1;

import java.util.*;

public class Message {
    public Message(List<String> strings) {
        lines = strings;
    }

    public void append(Message msg) {
        lines.addAll(msg.lines);
    }

    public List<String> getLines() {
        return lines;
    }

    private List<String> lines;
}
