/*
 * tuProlog - Copyright (C) 2001-2004  aliCE team at deis.unibo.it
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package alice.tuprologx.ide;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * A launcher for Swing <code>JFrame</code>s.
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.0 - 17-dic-02
 */

public class SwingFrameLauncher implements FrameLauncher {

    /** The path to the icon to be displayed on the title bar of each frame. */
    private String iconPathName = "";

    public void launchFrame(Component content) {
        this.launchFrame(content, "Frame", 0, 0);
    }

    public void launchFrame(Component content, String title, int width, int height) {
        final JFrame f = new JFrame(title);
        Container c = f.getContentPane();
        c.setLayout(new BorderLayout());
        c.add(content, BorderLayout.CENTER);
        f.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent event) {
                f.dispose();
            }
        });
        if (iconPathName != "") {
            // Set a title bar icon
            ImageIcon icon = new ImageIcon(getClass().getResource(getFrameIcon()));
            f.setIconImage(icon.getImage());
        }
        f.pack();
        if ((width > 0) && (height > 0)) {
            Insets insets = f.getInsets();
            width += insets.left + insets.right;
            height += insets.top + insets.bottom;
            Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            width = Math.min(width, screenSize.width);
            height = Math.min(height, screenSize.height);
            f.setBounds((screenSize.width - width) / 2, (screenSize.height - height) / 2, width, height);
        }
        f.setVisible(true);
    }

    public void setFrameIcon(String imagePathName) {
        iconPathName = imagePathName;
    }

    public String getFrameIcon() {
        return iconPathName;
    }

} // end SwingFrameLauncher class