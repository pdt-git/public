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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

import alice.tuprolog.InvalidTheoryException;
import alice.tuprolog.Prolog;
import alice.tuprolog.Theory;
import alice.util.thinlet.Thinlet;

/**
 * A set of commands and facilities for components implementing an edit area.
 * Bundled with an appropriate edit area, it forms the tuProlog editor in the
 * current <code>tuprologx.ide</code> package.
 *
 * @author  <a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version 1.0 - Friday 20th December, 2002
 */
public class ThinletTheoryEditor extends Thinlet implements PropertyChangeListener {

    /** The Prolog engine referenced by the editor. */
	private Prolog engine;
    /** The edit area used by the editor. */
    private TheoryEditArea editArea;
    /** A message describing the status of the console. */
    private String statusMessage;
    /** Used for components interested in changes of console's properties. */
    private PropertyChangeSupport propertyChangeSupport;

	public ThinletTheoryEditor() {
		try {
			add(parse("xml/ThinletTheoryEditor.xml"));
		} catch (Exception e) {
			e.printStackTrace();
		}
        propertyChangeSupport = new PropertyChangeSupport(this);
	}

    /**
     * Get the Prolog engine referenced by the editor.
     *
     * @return The Prolog engine referenced by the editor.
     */
	public Prolog getEngine() {
		return engine;
	}

    /**
     * Set the Prolog engine referenced by the editor.
     *
     * @param engine an <code>alice.tuprolog.Prolog</code> engine.
     */
	public void setEngine(Prolog engine) {
		this.engine = engine;
	}

    /**
     * Set the editor status.
     *
     * @param message The message describing the new status of the editor.
     */
    public void setStatusMessage(String message) {
        String oldStatusMessage = getStatusMessage();
        statusMessage = message;
        propertyChangeSupport.firePropertyChange("StatusMessage", oldStatusMessage, statusMessage);
    }

    /**
     * Get the editor status as a <code>java.lang.String</code> message.
     *
     * @return the current status of the editor as a <code>java.lang.String</code>
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
     * Set the edit area used by the editor to manipulate the text of
     * Prolog theories.
     *
     * @param editArea The edit area we want the editor to use.
     */
    public void setEditArea(TheoryEditArea editArea) {
        this.editArea = editArea;
    }

    /**
     * Set the theory of the tuProlog engine referenced by the editor to the
     * theory currently contained in the edit area.
     */
	public void setEngineTheory() {
        // insert a check on feededTheory? -> if true does not feed anything.
		String theory = editArea.getTheory();
		try {
			getEngine().setTheory(new Theory(theory));
			editArea.setDirty(false);
			setStatusMessage("New theory accepted.");
		} catch (InvalidTheoryException ite) {
			setStatusMessage("Error setting theory: Syntax Error at/before line " + ite.line);
		} 
	}
	
	/**
	 * Get the theory currently contained in the tuProlog engine referenced by
	 * the editor and display it in the edit area.
	 */
	public void getEngineTheory() {
		String theory = getEngine().getTheory().toString();
		editArea.setTheory(theory);
		setStatusMessage("Engine theory displayed.");
	}

	/**
     * Undo last action in the Edit Area.
     */
    public void undo() {
        editArea.undoAction();
    }

    /**
     * Redo last action in the Edit Area.
     */
    public void redo() {
        editArea.redoAction();
    }

    public void propertyChange(PropertyChangeEvent event) {
        String propertyName = event.getPropertyName();
        if (propertyName.equals("caretLine"))
            setCaretLine(event.getNewValue().toString());
    }

    /**
     * Display the line number where the caret in the edit area is.
     *
     * @param caretLine The line number to be displayed.
     */
    private void setCaretLine(String caretLine) {
        Object lineField = find("lineField");
        setString(lineField, "text", caretLine);
    }

    /**
     * Enable or disable theory-related buttons.
     *
     * @param flag true if the buttons have to be enabled, false otherwise.
     */
    protected void enableTheoryCommands(boolean flag) {
        Object setTheoryButton = find("setTheoryButton");
        setBoolean(setTheoryButton, "enabled", flag);
    }

} // end ThinletTheoryEditor class