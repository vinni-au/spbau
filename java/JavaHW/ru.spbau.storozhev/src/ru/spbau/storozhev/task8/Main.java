package ru.spbau.storozhev.task8;

/**
 * Main class for task #8 "Philosophers"
 * NOTE: this program always gets to deadlock, so it shall be interrupted manually.
 *
 * @author Anton Storozhev
 */
public class Main {
    public static void main(String[] args) {
        int n = 10;
        if (args.length >= 1) {
            System.out.println("Treating first parameter as N");
            n = Integer.valueOf(args[0]);
        }

        Table table = new Table(n);
        Philosopher philosopher = new Philosopher(table);
        for (int i = 0; i < n; ++i) {
            Thread thread = new Thread(philosopher);
            thread.setName((new Integer(i)).toString());
            thread.start();
        }

    }
}
