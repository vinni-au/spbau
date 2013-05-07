package ru.spbau.storozhev.task11;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.*;

/**
 * Viewer window
 *
 * @author Anton Storozhev
 */
public class ViewerWindow extends JFrame {

    private JTabbedPane tabbedPane;

    /**
     * Action to open file
     */
    class OpenAction extends AbstractAction {
        OpenAction() {
            putValue(Action.NAME, "Open...");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            JFileChooser fileChooser = new JFileChooser();
            fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

            int retVal = fileChooser.showOpenDialog(ViewerWindow.this);
            if (retVal == JFileChooser.APPROVE_OPTION) {
                addFile(fileChooser.getSelectedFile());
            }
        }
    }

    /**
     * Action to close current file
     */
    class CloseAction extends AbstractAction {
        CloseAction() {
            putValue(Action.NAME, "Close");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            int index = tabbedPane.getSelectedIndex();
            if (index != -1) {
                tabbedPane.removeTabAt(index);
            }
        }
    }

    /**
     * Action to exit program
     */
    class ExitAction extends AbstractAction {
        ExitAction() {
            putValue(Action.NAME, "Exit");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            System.exit(0);
        }
    }

    /**
     * Action to show information about program
     */
    class AboutAction extends AbstractAction {
        AboutAction() {
            putValue(Action.NAME, "About...");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            JOptionPane.showMessageDialog(ViewerWindow.this, "Simple text viewer", "About", JOptionPane.INFORMATION_MESSAGE);
        }
    }

    /**
     * Constructs window with greeting in the title
     * @param username name to greet
     */
    public ViewerWindow(String username) {
        super("Hello, " + username);

        createControls();
        setMinimumSize(new Dimension(150, 150));
    }

    /**
     * Loads file to viewer tab
     *
     * @param file file to load
     */
    private void addFile(File file) {
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            StringBuilder sb = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                sb.append(line);
                sb.append('\n');
            }
            JTextArea textField = new JTextArea();
            textField.setText(sb.toString());
            tabbedPane.add(file.getName(), textField);
            tabbedPane.setSelectedComponent(textField);
        } catch (FileNotFoundException exc) {
            System.err.println("File " + file.getName() + " wasn't found");
        } catch (IOException exc) {
            System.err.println("IO error while reading " + file.getName());
        }
    }

    /**
     * Creates form elements (buttons, menus etc.)
     */
    private void createControls() {
        OpenAction openAction = new OpenAction();
        CloseAction closeAction = new CloseAction();
        ExitAction exitAction = new ExitAction();
        AboutAction aboutAction = new AboutAction();

        JMenuBar mainMenu = new JMenuBar();
        JMenu fileMenu = new JMenu("File");
        fileMenu.add(openAction);
        fileMenu.add(closeAction);
        fileMenu.addSeparator();
        fileMenu.add(exitAction);

        JMenu helpMenu = new JMenu("Help");
        helpMenu.add(aboutAction);

        mainMenu.add(fileMenu);
        mainMenu.add(helpMenu);

        setJMenuBar(mainMenu);

        JButton btnOpen = new JButton("Open");
        JButton btnClose = new JButton("Close");
        JButton btnExit = new JButton("Exit");
        JButton btnAbout = new JButton("About");

        Box leftBox = Box.createVerticalBox();

        leftBox.add(btnOpen);
        leftBox.add(btnClose);
        leftBox.add(Box.createVerticalGlue());
        leftBox.add(btnAbout);
        leftBox.add(btnExit);

        btnOpen.addActionListener(openAction);
        btnClose.addActionListener(closeAction);
        btnExit.addActionListener(exitAction);
        btnAbout.addActionListener(aboutAction);

        tabbedPane = new JTabbedPane();

        getContentPane().add(leftBox, BorderLayout.WEST);
        getContentPane().add(tabbedPane);
    }
}
