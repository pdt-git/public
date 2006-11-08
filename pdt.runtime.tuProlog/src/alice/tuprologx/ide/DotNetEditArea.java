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

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.beans.*;

/**
 * An edit area for the .NET platform. Makes use of a plain
 * <code>java.awt.TextArea</code>.
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.0 - 13-nov-02
 */

public class DotNetEditArea extends Panel implements TheoryEditArea {

    /** The plain text component used by this edit area. */
    private TextArea textArea;
    /**
     * A table containing the incremental lengths of all the lines in
     * the <code>TextArea</code>.
     */
    private Vector table;
    /** The line in the text where the caret is currently positioned. */
    private int caretLine;
    /**
     * Indicate if the edit area is changed after the last Set Theory operation
     * issued by the editor.
     */
    private boolean dirty;
    /** Used for components interested in changes of console's properties. */
    private PropertyChangeSupport propertyChangeSupport;

    public DotNetEditArea() {
        textArea = new TextArea(25, 80);
        textArea.setFont(new Font("Courier", 0, 12));

        dirty = false;
        textArea.addKeyListener(new KeyAdapter() {
			public void keyReleased(KeyEvent e) {
                if (!dirty)
                    setDirty(true);
                inputKeyReleased(e);
			}
		});
        textArea.addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent e) {
                doSetCaretLine();
            }
        });
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
        add(textArea, constraints);

        // initialize the table
        table = buildLengthTable(textArea.getText());
        propertyChangeSupport = new PropertyChangeSupport(this);
    }

    public String getTheory() {
        return textArea.getText();
    }

    public void setTheory(String theory) {
        textArea.setText(theory);
    }

    public void setCaretLine(int caretLine) {
        int oldCaretLine = getCaretLine();
        this.caretLine = caretLine;
        propertyChangeSupport.firePropertyChange("caretLine", oldCaretLine, caretLine);
    }

    public int getCaretLine() {
        return caretLine;
    }

    public void setDirty(boolean flag) {
        dirty = flag;
    }

    public boolean isDirty() {
        return dirty;
    }

    /** Not implemented. */
    public void undoAction() {
    }

    /** Not implemented. */
    public void redoAction() {
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }

    /**
     * Handle the effects of a key pressed on the <code>TextArea</code>.
     *
     * @param e The <code>KeyEvent</code> to handle.
     */
    public void inputKeyReleased(KeyEvent e) {
        int keyCode = e.getKeyCode();
		if (!isCursorMovement(keyCode))
			table = buildLengthTable(textArea.getText());
		doSetCaretLine();
    }

    /**
     * Set the current caret line. This method is necessary in order to handle
     * different events in the same way.
     */
    private void doSetCaretLine() {
        int caretLine = getCurrentLine(textArea.getCaretPosition());
        setCaretLine(caretLine);
    }

    /**
     * Check if the effect of the key pressed is just a movement of the
     * caret, without any new character typed.
     *
     * @param keyCode The code of the key pressed.
     * @return true if the pressed key's effect is a cursor movement only,
     * false otherwise.
     */
    private boolean isCursorMovement(int keyCode) {
		switch (keyCode) {
			case 33: // Page Up
			case 34: // Page Down
			case 35: // End
			case 36: // Home
			case 37: // Left Arrow
			case 38: // Up Arrow
			case 39: // Right Arrow
			case 40: // Down Arrow
				return true;
			default:
				return false;
		}
	}

    /**
	 * Given the caret position in in the <code>TextArea</code>, return the
	 * line in the text where the caret is positioned.
     *
	 * @param caretPosition The position of the caret in the text.
	 * @return The line in the text where the caret is positioned.
	 */
	private int getCurrentLine(int caretPosition) {
		int textLength = ((Integer) table.get(table.size() - 1)).intValue();

		for (int index = 0; index < table.size(); index++) {
			int highBound = ((Integer) table.get(index)).intValue();
			if (caretPosition < highBound)
				return index + 1;
		}
		// return the correct line number if the caret is at the end of the whole text
		return table.size();
	}

	/**
	 * Build a table containing for each line the partial length of the text
	 * from the beginning to the current line.
     *
	 * @param text The text to analyze.
	 * @return The table as <code>java.util.Vector</code>.
	 */
	private Vector buildLengthTable(String text) {
		Vector table = new Vector();
		int lines = 0;
		int textLength = 0;
		String remaining = text;

		while (remaining.indexOf("\n") != -1) {
			int lineLength = remaining.substring(0, remaining.indexOf("\n")).length();
			if (lineLength == 0)
				textLength++;
			else
				textLength += lineLength + 1; // count the newline
			table.add(lines++, new Integer(textLength));
			remaining = remaining.substring(remaining.indexOf("\n") + 1);
		}
		// add the last line (the one not containing a newline)
		textLength += remaining.length();
		table.add(lines, new Integer(textLength));

		return table;
	}

} // end DotNetEditArea class