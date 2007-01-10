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

import java.awt.Component;

/**
 * A common interface to launchers of frames built with a particular GUI
 * toolkit. Note that this interface is not public, since it is intended for
 * internal use, and could be subject to several changes in future releases.
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.0 - 17-dic-02
 */

interface FrameLauncher {

    /**
     * Launch a frame containing the <code>Component</code> passed as an argument.
     *
     * @param c The <code>Component</code> contained in the launched frame.
     */
    public void launchFrame(Component content);

    /**
     * Launch a frame containing the <code>Component</code> passed as an argument,
     * and with specified title, width and height.
     *
     * @param c The <code>Component</code> contained in the launched frame.
     * @param title The title of the launched frame.
     * @param width The width of the launched frame.
     * @param height The height of the launched frame.
     */
    public void launchFrame(Component content, String title, int width, int height);

    /**
     * Set the path of the icon to be displayed on the title bar of each frame.
     *
     * @param imagePathName The path to image containing the title bar icon.
     */
    public void setFrameIcon(String imagePathName);

    /**
     * Get the path to the frame's title bar icon file.
     *
     * @return the path to the image containing title bar icon.
     */
    public String getFrameIcon();

} // end FrameLauncher interface