package ru.spbau.storozhev.task4;

import java.util.*;

/**
 * Represent an event
 */
public abstract class Event {
    /**
     * Returns whether event is ready
     * @return true if event is ready
     */
    public abstract boolean ready();

    /**
     * Fires event
     */
    public void fireEvent() {
        if (ready()) {
            for (ActionListener al : listeners) {
                al.performAction();
            }
        }
    }

    /**
     * Adds listener to current event
     * @param actionListener listener to add
     */
    public void addListener(ActionListener actionListener) {
        listeners.add(actionListener);
    }

    /**
     * Disables current event
     */
    public void disable() {
    }

    private List<ActionListener> listeners = new ArrayList<>();
}
