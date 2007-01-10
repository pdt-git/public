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

import alice.util.thinlet.Thinlet;

/**
 * A status bar for the tuProlog IDE.
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.0 - 15-dic-02
 */

public class ThinletStatusBar extends Thinlet implements PropertyChangeListener {

    public ThinletStatusBar() {
        try {
			add(parse("xml/ThinletStatusBar.xml"));
		} catch (Exception e) {
			e.printStackTrace();
		}
    }

    public void propertyChange(PropertyChangeEvent event) {
        String propertyName = event.getPropertyName();
        if (propertyName.equals("StatusMessage"))
            setStatusMessage(event.getNewValue().toString());
    }

    /**
     * Display the status message on the status bar.
     *
     * @param message The status message to be displayed.
     */
    public void setStatusMessage(String message) {
        Object statusTextField = find("statusTextField");
        setString(statusTextField, "text", message);
    }

} // end ThinletStatusBar class