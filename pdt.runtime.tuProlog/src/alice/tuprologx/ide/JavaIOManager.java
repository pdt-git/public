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

import alice.tuprolog.*;

import javax.swing.*;
import java.io.*;

/**
 * A manager for Input/Output operations on the Java 2 platform.
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.0 - 16-dic-02
 */

public class JavaIOManager extends IOFileOperations {

    /** The parent component to open the JFileChooser against. */
    private java.awt.Component parent;

    public JavaIOManager(java.awt.Component parent) {
        this.parent = parent;
    }

    public Theory loadTheory() throws Exception {
        JFileChooser chooser = new JFileChooser(currentLoadDirectory);
        chooser.setFileFilter(new PrologFileFilter());
        int returnVal = chooser.showOpenDialog(parent);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            currentLoadDirectory = chooser.getCurrentDirectory().toString();
            currentTheoryFileName = chooser.getCurrentDirectory() + File.separator + chooser.getSelectedFile().getName();
            return new Theory(new FileInputStream(currentTheoryFileName));
        } else
            return null;
    }

    public String saveTheoryAs(String theory) throws Exception {
        JFileChooser chooser = new JFileChooser(currentSaveDirectory);
        chooser.setDialogType(JFileChooser.SAVE_DIALOG);
        chooser.setFileFilter(new PrologFileFilter());
        int returnVal = chooser.showSaveDialog(parent);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            currentSaveDirectory = chooser.getCurrentDirectory().toString();
            currentTheoryFileName = chooser.getCurrentDirectory() + File.separator + chooser.getSelectedFile().getName();
            return save(theory);
        } else
            return "";
    }

    /**
     * A convenience implementation of FileFilter that filters out all files
     * except the ones supposed to be Prolog files, recognized by the ".pro"
     * or ".pl" extensions.
     *
     * @author  <a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
     * @version 1.0 - Monday 16th December, 2002
     */
    private class PrologFileFilter extends javax.swing.filechooser.FileFilter {

        /**
         * Return true if this file should be shown in the directory pane,
         * false if it shouldn't. Files that begin with "." are ignored.
         *
         * @see FileFilter#accept
         */
        public boolean accept(File f) {
            if (f != null) {
                if (f.isDirectory())
                    return true;
                String extension = getExtension(f);
                if (extension != null && (extension.equals("pl") || extension.equals("pro")))
                    return true;
            }
            return false;
        }

        /**
         * Return the extension portion of the file's name.
         *
         * @param f The file whose name we want the extension of.
         * @return The extension of the file's name.
         */
        private String getExtension(File f) {
            if (f != null) {
                String filename = f.getName();
                int i = filename.lastIndexOf('.');
                if (i > 0 && i < filename.length() - 1)
                    return filename.substring(i + 1).toLowerCase();
            }
            return null;
        }

        /**
         * Returns the human readable description of this filter.
         *
         * @return A human readable description of this filter.
         */
        public String getDescription() {
            return "Prolog files (*.pro, *.pl)";
        }

    } // end PrologFileFilter class
} // end JavaIOManager class