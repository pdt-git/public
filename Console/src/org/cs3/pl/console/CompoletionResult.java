package org.cs3.pl.console;
/*
 * Created on 06.10.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */



public interface CompoletionResult{
    public String getOriginalLineContent();
    public int getOriginalCaretPosition();
    public String getNewLineContent();
    public int getNewCaretPosition();
    public String[] getOptions();
}