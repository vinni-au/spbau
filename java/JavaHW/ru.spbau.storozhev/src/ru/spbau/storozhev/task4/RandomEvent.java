package ru.spbau.storozhev.task4;

import java.util.Random;

/**
 * Represents random event. It's ready with 50% possibility
 */
public class RandomEvent extends Event {
    /**
     * {@inheritDoc}
     */
    @Override
    public boolean ready() {
        if (random.nextInt() % 2 == 0)
            return true;
        return false;
    }

    private static Random random = new Random();
}
