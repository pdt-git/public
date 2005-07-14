package org.cs3.pl.console;


/**
 * used by the console completion to pass the results of the
 * completion processor.
 */
public interface CompoletionResult{
    public String getOriginalLineContent();
    public int getOriginalCaretPosition();
    public String getNewLineContent();
    public int getNewCaretPosition();
    public String[] getOptions();
}