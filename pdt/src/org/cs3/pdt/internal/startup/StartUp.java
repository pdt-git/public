package org.cs3.pdt.internal.startup;

import org.cs3.pl.common.Debug;
import org.eclipse.ui.IStartup;

public class StartUp implements IStartup {

		/* (non-Javadoc)
	 * @see org.eclipse.ui.IStartup#earlyStartup()
	 */
	public void earlyStartup() {
		Debug.info("To early to say anything concrete...");
	}

}
