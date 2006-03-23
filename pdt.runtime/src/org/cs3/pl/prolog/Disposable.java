package org.cs3.pl.prolog;

public interface Disposable {

    /**
     * Disposes the session. Any further call (except of further dispose calls
     * will cause the system to throw an IllegalStateException.
     */
    public void dispose();

    /**
     * checks if the session has been disposed. This can happen without the
     * users explicitly calling dispose, if for example restart() is called on
     * the PrologInterface.
     * 
     * @return true if the interface has been disposed, false otherwise
     */
    public boolean isDisposed();

}
