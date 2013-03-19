package ru.spbau.storozhev.task4;

import java.util.Timer;

public class TimeEvent extends Event {
    public TimeEvent() {
    }

    @Override
    public boolean ready() {
        return false;
    }

    private Timer timer = new Timer();
}
