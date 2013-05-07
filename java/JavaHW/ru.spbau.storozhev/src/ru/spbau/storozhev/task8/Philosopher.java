package ru.spbau.storozhev.task8;

import java.util.Random;
import java.util.concurrent.TimeUnit;

/**
 * Represents Philosopher who can eat and think
 *
 * @author Anton Storozhev
 */
public class Philosopher implements Runnable {
    private Table table;
    private State state = State.THINKING;
    private static final Random rand = new Random();

    /**
     * State of the philosopher
     */
    private enum State {
        THINKING,
        EATING
    }

    /**
     * Constructs a Philoshopher and plant him to given table
     * @param table table where philosopher will sit
     */
    public Philosopher(Table table) {
        this.table = table;
    }

    /**
     * Lifecycle of philosopher: decide what to do - eat or think and do it
     */
    @Override
    public void run() {
        int number = Integer.valueOf(Thread.currentThread().getName());
        System.out.println(number + ". I'm here!");
        try {
            while (true) {
                if (state == State.THINKING) {
                    System.out.println(number + ". I'm thinking!");
                    TimeUnit.MILLISECONDS.sleep(15);
                }

                if (state == State.EATING) {
                    Stick leftStick = table.getStick(number);
                    synchronized (leftStick) {
                        System.out.println(number + ". Got left stick!");
                        Stick rightStick = table.getStick((number + 1) % table.stickCount());
                        synchronized (rightStick) {
                            System.out.println(number + ". Got right stick!");
                            System.out.println(number + ". I'm eating!");
                            TimeUnit.MILLISECONDS.sleep(10);
                        }
                    }
                    System.out.println(number + ". Done eating!");
                }

                if (Math.abs(rand.nextInt() % 2) == 0) {
                    state = State.THINKING;
                } else {
                    state = State.EATING;
                }
            }
        } catch (InterruptedException exc) {
            System.err.println("Unfortunately, philosopher #" + number + " was interrupted");
        }
    }
}
