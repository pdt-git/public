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

import alice.tuprolog.event.SpyEvent;
import alice.tuprolog.event.SpyListener;
import alice.tuprolog.event.WarningEvent;
import alice.tuprolog.event.WarningListener;
import alice.util.thinlet.Thinlet;

/**
 * A simple area where to show debug messages.
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.0 - 15-dic-02
 */

public class ThinletDebugArea extends Thinlet implements SpyListener, WarningListener {

    public ThinletDebugArea() {
        try {
			add(parse("xml/ThinletDebugArea.xml"));
		} catch (Exception e) {
			e.printStackTrace();
		}
    }

    public void onSpy(SpyEvent event) {
        Object debugArea = find("debugArea");
        String debugMessage = getString(debugArea, "text");
        if (event.getSnapshot()==null) {
        		setString(debugArea, "text", debugMessage + event.getMsg() + "\n");
        } else {
    			setString(debugArea, "text", debugMessage + event.getMsg() + "\n" + event.getSnapshot().toString() + "\n\n");
        }
    }

	public void onWarning(WarningEvent event) {
		Object debugArea = find("debugArea");
		String debugMessage = getString(debugArea, "text");
		setString(debugArea, "text", debugMessage + event.getMsg() + "\n");
	}

    /**
     * Clear the debug area.
     */
    public void clear() {
        Object debugArea = find("debugArea");
        setString(debugArea, "text", "");
    }

} // end ThinletDebugArea class