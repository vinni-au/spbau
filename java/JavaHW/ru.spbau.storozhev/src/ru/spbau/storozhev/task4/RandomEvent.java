package ru.spbau.storozhev.task4;

import java.util.Random;

public class RandomEvent extends Event {
    @Override
    public boolean ready() {
        if (random.nextInt() % 2 == 0)
            return true;
        return false;
    }

    private static Random random = new Random();
}
