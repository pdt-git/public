package org.cs3.pl.parser;


public interface TaskCollector {
	public void reportTask(Task task);
    
    /**
     * called by the prolog compiler before parsing of the file.
     * Implementations may use this opertunity to clear all error state there may exist for
     * what ever is compiled... e.g. problem markers for a given resource (hint, hint) :-)
     */
    public void reset();
    
    /**
     * Called by the prolog compiler when the completion finished.
     * Implementations may use this opertunity to do bulk reports
     * of collected problems.
     */
    public void done();
}
