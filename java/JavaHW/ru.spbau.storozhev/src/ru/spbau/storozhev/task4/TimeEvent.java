package ru.spbau.storozhev.task4;

import java.util.Timer;
import java.util.TimerTask;

/**
 * Represents event which is ready every 10 seconds
 */
public class TimeEvent extends Event {
    /**
     * Constructs a time event
     */
    public TimeEvent() {
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                ready = true;
            }
        }, 10000, 10000);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean ready() {
        return ready;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fireEvent() {
        super.fireEvent();
        ready = false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disable() {
        timer.cancel();
    }

    private boolean ready = true;
    private Timer timer = new Timer();

}
