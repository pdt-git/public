package org.cs3.pl.metadata;

import org.cs3.pl.cterm.CTerm;

/**
 * WARNING: this is work in progress. 
 * 
 * 
 * @author lukas
 *
 */
public interface Goal {

	/**@deprecated*/
	public int getArity();

	/**@deprecated*/
	public String getName();

	public String getModule();

	public CTerm getTerm();
}