package ru.spbau.storozhev.task11;

import java.awt.image.BufferedImage;
import java.io.*;
import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Login window
 *
 * @author Anton Storozhev
 */
public class LoginWindow extends JFrame {

    private JFrame applicationWindow;

    private JProgressBar progressBar;

    private JTextField loginField;
    private JPasswordField passwordField;

    /**
     * Action listener to clear fields
     */
    private ActionListener clearActionListener = new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
            loginField.setText("");
            passwordField.setText("");
        }
    };

    /**
     * Action listener to perform login
     */
    private ActionListener loginActionListener = new ActionListener () {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (loginField.getText().isEmpty()) {
                JOptionPane.showMessageDialog(LoginWindow.this, "Login is empty!", "Error", JOptionPane.ERROR_MESSAGE);
                return;
            }

            for (int i = 1; i <= 100; ++i) {
                final int percentage = i;
                try {
                    SwingUtilities.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            progressBar.setValue(percentage);
                        }
                    });

                    Thread.sleep(30);
                } catch (InterruptedException exc) {
                    System.err.println("Thread was interrupted unexpectedly");
                }
            }

            applicationWindow = new ViewerWindow(loginField.getText());
            applicationWindow.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
            LoginWindow.this.setVisible(false);
            applicationWindow.setVisible(true);
        }
    };

    /**
     * Action listener to perform registration
     */
    private ActionListener registerActionListener = new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
            JOptionPane.showMessageDialog(LoginWindow.this, "OK", "", JOptionPane.INFORMATION_MESSAGE);
        }
    };

    /**
     * Constructs login window
     */
    public LoginWindow() {
        super("Login");
        setMinimumSize(new Dimension(350, 160));

        createControls();
    }

    /**
     * Creates form elements (buttons etc.)
     */
    private void createControls() {
        progressBar = new JProgressBar(JProgressBar.HORIZONTAL);
        loginField = new JTextField();
        passwordField = new JPasswordField();
        Panel inputsPanel = new Panel();
        BufferedImage image = null;
        try {
            image = ImageIO.read(new File("pic.png"));
        } catch (IOException e) {
            System.err.println("Couldn't load image: pic.png");
        }
        PicturePanel picPanel = new PicturePanel(image);

        JButton btnClear = new JButton("Clear");
        JButton btnRegister = new JButton("Register");
        JButton btnLogin = new JButton("Login");

        btnClear.addActionListener(clearActionListener);
        btnLogin.addActionListener(loginActionListener);
        btnRegister.addActionListener(registerActionListener);

        Box centerBox = Box.createHorizontalBox();
        Box rightBox = Box.createVerticalBox();
        Box inputsBox = Box.createVerticalBox();
        Box buttonsBox = Box.createHorizontalBox();

        buttonsBox.add(btnClear);
        buttonsBox.add(btnRegister);
        buttonsBox.add(btnLogin);

        inputsPanel.setLayout(new GridLayout(2, 2));
        inputsPanel.add(new JLabel("Login:", JLabel.RIGHT));
        inputsPanel.add(loginField);
        inputsPanel.add(new JLabel("Password:", JLabel.RIGHT));
        inputsPanel.add(passwordField);
        inputsPanel.setMaximumSize(new Dimension(4000, 40));

        inputsBox.add(inputsPanel);

        rightBox.add(inputsBox);
        rightBox.add(Box.createVerticalGlue());
        rightBox.add(buttonsBox);

        centerBox.add(picPanel);
        centerBox.add(rightBox);

        getContentPane().add(centerBox);
        getContentPane().add(progressBar, BorderLayout.SOUTH);
    }

}
