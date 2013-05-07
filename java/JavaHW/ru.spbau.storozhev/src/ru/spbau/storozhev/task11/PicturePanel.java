package ru.spbau.storozhev.task11;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;

/**
 * Panel with an auto-resizable picture on it
 *
 * @author Anton Storozhev
 */
public class PicturePanel extends JPanel {

    private BufferedImage image;

    /**
     * Constructs picture panel with the given image
     * @param image image to be drawn on the panel
     */
    public PicturePanel(BufferedImage image) {
        super();
        this.image = image;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        if (image != null) {
            final int minDim = Math.min(getWidth(), getHeight());
            g.drawImage(image, getWidth() - minDim, getHeight() - minDim, minDim, minDim, null);
        }
    }
}
