package ru.spbau.storozhev.task8;

import java.util.ArrayList;
import java.util.List;

/**
 * Table where sticks are laying
 *
 * @author Anton Storozhev
 */
public class Table {
    private List<Stick> sticks = new ArrayList<Stick>();

    /**
     * Constructs table with given number of sticks
     * @param n number of sticks to create
     */
    public Table(int n) {
        for (int i = 0; i < n; ++i) {
            sticks.add(new Stick());
        }
    }

    /**
     * Returns stick with given number
     * @param i number of stick
     * @return stick with given number
     */
    public Stick getStick(int i) {
        return sticks.get(i);
    }

    /**
     * Returns stick count
     * @return stick count
     */
    public int stickCount() {
        return sticks.size();
    }

}
