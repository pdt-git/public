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

import java.awt.FileDialog;
import java.awt.Frame;
import java.io.File;
import java.io.FileInputStream;

import alice.tuprolog.Theory;

/**
 * A manager for Input/Output file operations on the .NET platform. Note that
 * FileDialog use a very naive file filtering method, or does not use it at all,
 * since the preferred method to filter files in AWT, namely a custom implementation
 * of the FilenameFilter interface, is said not to work on the Windows OS (see
 * the JavaDocs for the FileDialog class in the official JDK documentation).
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.0 - 16-dic-02
 */

public class DotNetIOManager extends IOFileOperations {

    /** The parent frame to open the FileDialog against. */
    private Frame parent;

    public DotNetIOManager(Frame parent) {
        this.parent = parent;
    }

    public Theory loadTheory() throws Exception {
        FileDialog dialog = new FileDialog(parent, "Load Theory", FileDialog.LOAD);
        dialog.setDirectory(currentLoadDirectory);
        dialog.setFile("*.pro");
        dialog.show();
        String directory = dialog.getDirectory();
        currentLoadDirectory = directory;
        String fileName = dialog.getFile();
        if ((directory != null) && (fileName != null)) {
            currentTheoryFileName = directory + File.separator + fileName;
            return new Theory(new FileInputStream(currentTheoryFileName));
        }
        else
            return null;
    }

    public String saveTheoryAs(String theory) throws Exception {
        FileDialog dialog = new FileDialog(parent, "Save Theory As...", FileDialog.SAVE);
        dialog.setDirectory(currentSaveDirectory);
        dialog.show();
        String directory = dialog.getDirectory();
        currentSaveDirectory = directory;
        String fileName = dialog.getFile();
        if (directory != null && fileName != null) {
            currentTheoryFileName = directory + File.separator + fileName;
            return save(theory);
        } else
            return "";
    }

} // end DotNetIOManager class