package org.cs3.pl.prolog;

import org.cs3.pl.prolog.internal.AbstractPrologInterface;

/**
 * A pluggable strategy for starting and stopping a PIF server.
 * 
 * Sincethe Prolog System is living in its own process for 
 * all the implemntations we tried until now, and starting 
 * and stopping this "server" process tends to be dependent
 * on a lot of enviremental issues (Do i run as a eclipse plugin?
 * Am i running on a windoze box? ... ), I decided to move 
 * some of this problems into a separate pluggable object.   
 *  
 */
public interface ServerStartAndStopStrategy {
    /**
     * starts the server process, returning its process.
     * 
     * @return the server process, if available, or null.
     * @param pif
     *                    the PrologInterface for which the server part should be
     *                    started.
     */
    public Process startServer(PrologInterface pif);

    /**
     * stop the server process
     * 
     * @param pif
     *                    the IPrologInterface for which the server should be stopped.
     * @param now
     *                    Normaly we should allow the server process to terminate
     *                    gracefully. if this flag is set, the implementation should
     *                    forget all politeness and "simply do it". This is ment as a
     *                    hint. Wether this flag has any effect depends on the concrete
     *                    implementation.
     */
    public void stopServer(PrologInterface pif, boolean now);

    /**
     * @param interface1
     * @return
     */
    public boolean isRunning(PrologInterface pif);
}
