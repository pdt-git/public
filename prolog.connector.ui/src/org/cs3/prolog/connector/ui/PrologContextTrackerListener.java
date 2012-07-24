/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.connector.ui;

import java.util.EventListener;

public interface PrologContextTrackerListener extends EventListener {
	public void contextChanged(PrologContextTrackerEvent e);
}

