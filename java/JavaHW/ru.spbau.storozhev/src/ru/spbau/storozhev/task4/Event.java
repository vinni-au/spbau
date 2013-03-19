package ru.spbau.storozhev.task4;

import java.util.*;

public abstract class Event {
    public abstract boolean ready();

    public void fireEvent() {
        if (ready()) {
            for (ActionListener al : listeners) {
                al.performAction();
            }
        }
    }

    public void addListener(ActionListener actionListener) {
        listeners.add(actionListener);
    }

    private List<ActionListener> listeners;
}
