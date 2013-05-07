package ru.spbau.storozhev.task11;

import javax.swing.*;

/**
 * Main class for simple text viewer
 *
 * @author Anton Storozhev
 */
public class Main {

    /**
     * Main method for the program
     * @param args
     */
    public static void main(String[] args) {
        LoginWindow loginWindow = new LoginWindow();
        loginWindow.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        loginWindow.setVisible(true);
    }
}
