/*
 * Created on 10.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.regenerator;


public interface IAffectedFile {

    public final int CREATED = 1;

    public final int CHANGED = 2;

    public final int REMOVED = 3;

    public int getStatus();

    public String getFilename();

    public ITextChange[] getTextChanges();

}