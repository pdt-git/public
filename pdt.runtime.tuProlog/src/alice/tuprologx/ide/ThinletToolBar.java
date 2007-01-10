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

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

import alice.tuprolog.Prolog;
import alice.tuprolog.Theory;
import alice.util.thinlet.Thinlet;

/**
 * A toolbar for the tuProlog IDE.
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.0 - 15-dic-02
 */

public class ThinletToolBar extends Thinlet {

    /** The Prolog engine referenced by the toolbar. */
    private Prolog engine;
    /** The debug area launched by the toolbar. */
    private Thinlet debugArea;
    /** The Library dialog launched by the toolbar. */
    private LibraryDialog libraryDialog;
    /** The file manager launched by the toolbar. */
    private IOFileOperations fileManager;
    /** A frame launcher used to launch and display dialogs and new windows. */
    private FrameLauncher frameLauncher;
    /** A message describing the status of the console. */
    private String statusMessage;
    /** Used for components interested in changes of console's properties. */
    private PropertyChangeSupport propertyChangeSupport;
    
    /**
	 * The IDE the toolbar belongs to, necessary to manage editor-related
	 * commands such as saving its content to the filesystem.
	 */
	private IDE ide;

    public ThinletToolBar(IDE ide) {
        try {
			add(parse("xml/ThinletToolBar.xml"));
		} catch (Exception e) {
			e.printStackTrace();
		}
        propertyChangeSupport = new PropertyChangeSupport(this);
        this.ide = ide;
    }

    /**
     * Get the Prolog engine referenced by the toolbar.
     *
     * @return The Prolog engine referenced by the toolbar.
     */
    public Prolog getEngine() {
		return engine;
	}

    /**
     * Set the Prolog engine referenced by the toolbar.
     *
     * @param engine an <code>alice.tuprolog.Prolog</code> engine.
     */
	public void setEngine(Prolog engine) {
		this.engine = engine;
	}


    /**
     * Set the toolbar status.
     *
     * @param message The message describing the new status of the toolbar.
     */
    public void setStatusMessage(String message) {
        String oldStatusMessage = getStatusMessage();
        statusMessage = message;
        propertyChangeSupport.firePropertyChange("StatusMessage", oldStatusMessage, statusMessage);
    }

    /**
     * Get the toolbar status as a <code>java.lang.String</code> message.
     *
     * @return the current status of the toolbar as a <code>java.lang.String</code>
     * message.
     */
    public String getStatusMessage() {
        return statusMessage;
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }

    /**
     * Set the file manager referenced by the toolbar for use in Input/Output tasks.
     *
     * @param fileManager The file manager we want the toolbar to use.
     */
    public void setFileManager(IOFileOperations fileManager) {
        this.fileManager = fileManager;
    }

    /**
     * Get the file manager used by the toolbar.
     *
     * @return The file manager used by the toolbar.
     */
    public IOFileOperations getFileManager() {
        return fileManager;
    }

    /**
     * Set the frame launcher referenced by the toolbar for use in displaying
     * dialogs and new windows.
     *
     * @param frameLauncher The frame launcher we want the toolbar to use.
     */
    public void setFrameLauncher(FrameLauncher frameLauncher) {
        this.frameLauncher = frameLauncher;
    }

    /**
     * Get the frame launcher used by the toolbar.
     *
     * @return The frame launcher used by the toolbar.
     */
    public FrameLauncher getFrameLauncher() {
        return frameLauncher;
    }

    /**
     * Enable or disable theory-related buttons.
     *
     * @param flag true if the buttons have to be enabled, false otherwise.
     */
    protected void enableTheoryCommands(boolean flag) {
        Object newTheoryButton = find("newTheoryButton");
		setBoolean(newTheoryButton, "enabled", flag);
        Object loadTheoryButton = find("loadTheoryButton");
        setBoolean(loadTheoryButton, "enabled", flag);
        Object saveTheoryButton = find("saveTheoryButton");
        setBoolean(saveTheoryButton, "enabled", flag);
        Object saveTheoryAsButton = find("saveTheoryAsButton");
        setBoolean(saveTheoryAsButton, "enabled", flag);
        /* disable also the Library Manager button, since each
         * library can have its theory to be feeded into the engine. */
        Object openLibraryManagerButton = find("openLibraryManagerButton");
        setBoolean(openLibraryManagerButton, "enabled", flag);
    }

    /**
     * Reset the engine's theory to a new blank theory.
     */
    public void newTheory() {
        engine.clearTheory();
		ide.setEditorContent("");
        fileManager.resetDefaultTheoryFileName();
        setStatusMessage("Ready (new theory accepted).");
    }

    /**
     * Load the engine with a theory previously stored in a file.
     */
    public void loadTheory() {
        setStatusMessage("Load Theory...");
        try {
            Theory theory = fileManager.loadTheory();
            if (theory != null) {
                engine.setTheory(theory);
                ide.setEditorContent(theory.toString());
                setStatusMessage("Ready (theory consulted).");
            } else
                setStatusMessage("Ready.");
        } catch (Exception e) {
            setStatusMessage("Error consulting theory.");
        }
    }

    /**
     * Save the engine's current theory to a default file.
     */
    public void saveTheory() {
        try {
        	String theoryFileName = fileManager.saveTheory(ide.getEditorContent());
            setStatusMessage("Theory saved to " + theoryFileName + ".");
        } catch (Exception exception){
            setStatusMessage("Error saving theory.");
        }
    }

    /**
     * Save the engine's current theory to a file chosen by the user.
     */
    public void saveTheoryAs() {
        setStatusMessage("Save Theory As...");
        try {
        	String theoryFileName = fileManager.saveTheoryAs(ide.getEditorContent());
            if (theoryFileName != "")
                setStatusMessage("Theory saved to " + theoryFileName + ".");
            else
                setStatusMessage("Ready.");
        } catch (Exception e) {
            setStatusMessage("Error saving theory.");
        }
    }

    /**
     * Opens the Library Manager dialog.
     */
    public void openLibraryManager() {
        frameLauncher.launchFrame(libraryDialog, "Library Manager", 275, 250);
    }

    /**
     * Show an instance of the debug area referenced by the toolbar in order to
     * display debug informations.
     */
    public void viewDebugInformation() {
        frameLauncher.launchFrame(debugArea, "Debug Information", 200, 300);
    }

    /**
     * Display an About dialog with information on the system and its crafters.
     */
    public void viewAboutInformation() {
        Thinlet aboutThinlet = new Thinlet();
        try {
            aboutThinlet.add(parse("xml/AboutDialog.xml"));
        } catch (Exception e) {
            e.printStackTrace();
        }
        Object tuPrologSystemLabel = aboutThinlet.find("tuPrologSystem");
        // Display the tuProlog System version
        aboutThinlet.setString(tuPrologSystemLabel, "text", "tuProlog System version " + Prolog.getVersion());
        frameLauncher.launchFrame(aboutThinlet, "About tuProlog IDE", 270, 125);
    }

    /**
     * Set the debug area referenced by the toolbar for use in displaying debug
     * informations.
     *
     * @param debugArea The debug area we want the toolbar to display.
     */
    public void setDebugArea(Thinlet debugArea) {
        this.debugArea = debugArea;
    }

    /**
     * Get the debug area used by the toolbar.
     *
     * @return The debug area used by the toolbar.
     */
    public Thinlet getDebugArea() {
        return debugArea;
    }

    /**
     * Set the library dialog referenced by the toolbar for use in displaying library
     * management facilities.
     *
     * @param libraryDialog The library manager dialog we want the toolbar to display.
     */
    public void setLibraryDialog(LibraryDialog libraryDialog) {
        this.libraryDialog = libraryDialog;
    }

} // end ThinletToolBar class