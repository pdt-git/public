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
import javax.swing.undo.*;
import javax.swing.event.*;
import java.beans.*;
import java.awt.event.*;

/**
 * An edit area for the Java 2 platform. Makes use of an advanced Swing text area.
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.0 - 14-nov-02
 */

public class JavaEditArea extends JPanel implements TheoryEditArea {

    /** The advanced Swing text area used by this edit area. */
    private alice.util.jedit.JEditTextArea inputTheory;
    /**
     * The line number corresponding to the caret's current position in the
     * text area.
     */
    private int caretLine;
    /**
     * Indicate if the edit area is changed after the last Set Theory operation
     * issued by the editor.
     */
    private boolean dirty;
    /** Used for components interested in changes of console's properties. */
    private PropertyChangeSupport propertyChangeSupport;
    /** Undo Manager for the Document in the JEditTextArea. */
    private UndoManager undoManager;

    public JavaEditArea() {
        PrologTextArea textArea = new PrologTextArea();

        setKeyBindings(textArea);

        inputTheory = new alice.util.jedit.JEditTextArea(textArea);
        inputTheory.setTokenMarker(new PrologTokenMarker());
        setLayout(new java.awt.GridBagLayout());
        java.awt.GridBagConstraints constraints = new java.awt.GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridheight = java.awt.GridBagConstraints.REMAINDER;
        constraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        constraints.fill = java.awt.GridBagConstraints.BOTH;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.insets = new java.awt.Insets(0, 0, 10, 0);

        inputTheory.addCaretListener(new CaretListener() {
            public void caretUpdate(CaretEvent event) {
                setCaretLine(inputTheory.getCaretLine() + 1);
            }
        });

        dirty = false;
        inputTheory.getDocument().addDocumentListener(new DocumentListener() {
            public void insertUpdate(DocumentEvent event) {
                changedUpdate(event);
            }
            public void removeUpdate(DocumentEvent event) {
                changedUpdate(event);
            }
            public void changedUpdate(DocumentEvent event) {
                if (!dirty)
                    setDirty(true);
            }

        });

        undoManager = new UndoManager();
        inputTheory.getDocument().addUndoableEditListener(undoManager);

        add(inputTheory, constraints);
        propertyChangeSupport = new PropertyChangeSupport(this);
    }

    /**
     * Set key bindings for edit area actions. Note that the key bindings must
     * be added to the inputHandler in the PrologTextArea through the facilities
     * offered by this component from the jEdit package.
     */
    private void setKeyBindings(PrologTextArea textArea) {
        // C+Z == Ctrl + Z
        textArea.inputHandler.addKeyBinding("C+Z", new AbstractAction() {
            public void actionPerformed(ActionEvent event) {
                undoAction();
            }
        });
        // CS+Z = Ctrl + Shift + Z
        textArea.inputHandler.addKeyBinding("CS+Z", new AbstractAction() {
            public void actionPerformed(ActionEvent event) {
                redoAction();
            }
        });
    }

    public void setCaretLine(int caretLine) {
        int oldCaretLine = getCaretLine();
        this.caretLine = caretLine;
        propertyChangeSupport.firePropertyChange("caretLine", oldCaretLine, caretLine);
    }

    public int getCaretLine() {
        return caretLine;
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }

    public void setTheory(String theory) {
        inputTheory.setText(theory);
    }

    public String getTheory() {
        return inputTheory.getText();
    }

    public void setDirty(boolean flag) {
        dirty = flag;
    }

    public boolean isDirty() {
        return dirty;
    }

    /* Managing Undo/Redo actions. */

    public void undoAction() {
        try {
            undoManager.undo();
        } catch (CannotUndoException e) {
            // e.printStackTrace();
        }
    }

    public void redoAction() {
        try {
            undoManager.redo();
        } catch (CannotRedoException e) {
            // e.printStackTrace();
        }
    }

} // end JavaEditArea class