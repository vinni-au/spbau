package ru.spbau.storozhev.task4;

import java.text.SimpleDateFormat;
import java.util.Calendar;

/**
 * Main class for task 4
 */
public class Main {
    /**
     * Program entry point.
     * @param args arguments of the program. not used
     */
    public static void main(String[] args) {
        RandomEvent randomEvent = new RandomEvent();
        TimeEvent timeEvent = new TimeEvent();
        int i = 0;
        for (; i < 5; ++i) {
            final Event event = i > 2 ? randomEvent : timeEvent;
            final int n = i;

            event.addListener(new ActionListener() {
                @Override
                public void performAction() {
                    System.out.println(event.getClass().getSimpleName() + " listener #" + n);
                }
            });
        }

        for (; i < 10; ++i) {
            final Event event = i > 7 ? randomEvent : timeEvent;
            final int n = i;

            event.addListener(new ActionListener() {
                String timeString = new SimpleDateFormat("HH:mm:ss").format(Calendar.getInstance().getTime());

                @Override
                public void performAction() {
                    System.out.println(timeString);
                }
            });
        }

        for (int j = 0; j < 20; ++j) {
            System.out.println("-----------------");
            randomEvent.fireEvent();
            timeEvent.fireEvent();
            try {
                //for TimeEvents
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                System.err.println("Thread was interrupted");
            }
        }

        randomEvent.disable();
        timeEvent.disable();
    }
}
