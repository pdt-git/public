package org.cs3.pl.prolog;

import java.util.EventListener;

/**
 * Lister for Prolog Session Output. 
 * @author schulzs1
 */
public interface OutputListener extends EventListener {
	/**
	 * called whenever new Prolog output becomes available. the output consists of a
	 * single line and does not include a newline character.
	 * @param s
	 */
	public abstract void onOutput(String s);
}
