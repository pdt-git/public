package org.cs3.pl.prolog;
public interface ShutdownHook {
    /**
     * called before the prolog interface is shut down.
     *
     * The PrologInterface is guaranteed to be fully functional
     * for the duration of this call, so no extra session is needed.
     * Just use the interface as usual.
     */
    public void beforeShutDown(PrologSession session);
}
