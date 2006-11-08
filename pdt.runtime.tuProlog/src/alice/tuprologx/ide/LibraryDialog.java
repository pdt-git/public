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

import alice.tuprolog.InvalidLibraryException;
import alice.tuprolog.event.LibraryEvent;
import alice.tuprolog.event.LibraryListener;
import alice.util.thinlet.Thinlet;

/**
 * A dialog providing an UI for the Library Manager.
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	2.0 - 27-may-05
 */

public class LibraryDialog extends Thinlet implements LibraryListener {

    /** The library manager associated with the interface. */
    private LibraryManager libraryManager;

    public LibraryDialog(LibraryManager libraryManager) {
        try {
			add(parse("xml/LibraryDialog.xml"));
		} catch (Exception e) {
			e.printStackTrace();
		}
        this.libraryManager = libraryManager;
        displayLibraryManagerStatus();
        // setStatusMessage("Ready.");
    }

    /**
     * Build the dialog displaying the name of the managed libraries and
     * the status for each of the managed libraries.
     */
    private void displayLibraryManagerStatus() {
        Object[] libraries = libraryManager.getLibraries();
        for (int i = 0; i < libraries.length; i++) {
            createTextField(libraries[i]);
            createComboBox(libraries[i]);
        }
    }

    /**
     * Create a textfield in the Thinlet panel to display the name of a
     * library managed by the Library Manager.
     *
     * @param library A library in the Library Manager.
     */
    private void createTextField(Object library) {
        Object textfield = create("textfield");
        add(find("librariesDisplay"), textfield, 0);
        String libraryClassname = library.toString();
        String libraryName = libraryClassname.substring(libraryClassname.lastIndexOf('.') + 1, libraryClassname.length());
        setString(textfield, "text", libraryName);
        setInteger(textfield, "weightx", 90);
    }

    /**
     * Create a combobox in the Thinlet panel to display the status of a
     * library managed by the Library Manager.
     *
     * @param library A library in the Library Manager.
     */
    private void createComboBox(Object library) {
        Object combobox = create("combobox");
        add(find("librariesDisplay"), combobox, 1);
        setBoolean(combobox, "editable", false);
        Object choiceLoaded = create("choice");
        add(combobox, choiceLoaded, 0);
        setString(choiceLoaded, "text", "Loaded");
        Object choiceUnloaded = create("choice");
        add(combobox, choiceUnloaded, 1);
        setString(choiceUnloaded, "text", "Unloaded");
        boolean selected = libraryManager.isLibraryLoaded(library.toString());
		setInteger(combobox, "selected", selected ? 0 : 1);
	    setInteger(combobox, "weightx", 10);
    }

    /**
     * Set the status message in the status bar of this dialog.
     *
     * @param message The message to set in the status bar.
     */
    private void setStatusMessage(String message) {
        setString(find("statusTextField"), "text", message);
    }

    /**
     * Add a library from the dialog to the manager.
     *
     * @param libraryClassname The name of the .class of the library to be added.
     */
    public void addLibrary(String libraryClassname) {
        try {
            libraryManager.addLibrary(libraryClassname);
            removeAll(find("librariesDisplay"));
            displayLibraryManagerStatus();
            setStatusMessage("Ready.");
        } catch (ClassNotFoundException e) {
            setStatusMessage(libraryClassname + ": Class Not Found");
        } catch (InvalidLibraryException e) {
            setStatusMessage(libraryClassname + ": Not a Library");
        }
    }

    /**
     * Load and unload the managed libraries following the indications given
     * by the user through the dialog interface.
     */
    public void setLibraryManagerStatus() {
        Object[] items = getItems(find("librariesDisplay"));
        Object[] libraries = libraryManager.getLibraries();
        for (int i = 0; i < items.length; i += 2) {
            String libraryClassname = libraries[i / 2].toString();
            String library = getString(items[items.length - i - 2], "text");
            /* TODO: Check that the library corresponds to the classname?
             * We are assuming that objects in the two arrays are stored
             * so that the order of items[] is the reversed order of
             * libraries[], and we do not perform any check... */
            String status = getString(items[items.length - i - 1], "text");
            try {
                if (status.equals("Loaded"))
                    libraryManager.loadLibrary(libraryClassname);
                if (status.equals("Unloaded"))
                    libraryManager.unloadLibrary(libraryClassname);
                closeLibraryDialog();
            } catch (InvalidLibraryException e) {
                setStatusMessage(e.getMessage());
            } 
        }
    }

    /**
     * Close the Library Dialog.
     */
    public void closeLibraryDialog() {
        java.awt.Component c = getParent();
        while (!(c instanceof java.awt.Window))
            c = c.getParent();
        ((java.awt.Window) c).dispose();
    }

	/** @see alice.tuprolog.event.LibraryListener#libraryLoaded(alice.tuprolog.event.LibraryEvent) */
	public void libraryLoaded(LibraryEvent event) {
		String libraryName = event.getLibraryName();
		if (!libraryManager.contains(libraryName))
			addLibrary(libraryName);
		else {
			removeAll(find("librariesDisplay"));
			displayLibraryManagerStatus();
		}
	}

	/** @see alice.tuprolog.event.LibraryListener#libraryUnloaded(alice.tuprolog.event.LibraryEvent) */
	public void libraryUnloaded(LibraryEvent event) {
		String libraryName = event.getLibraryName();
		removeAll(find("librariesDisplay"));
		displayLibraryManagerStatus();
	}
	
} // end LibraryDialog class