/*
 */
package org.cs3.pl.prolog.internal;

import java.io.IOException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.ServerStartStrategy;

/**
 */
public class InProcessStartStrategy implements ServerStartStrategy {

    
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.ServerStartStrategy#startServer(int)
     */
    public Process startServer(int port) {
        try {
            PrologInterfaceServer.main(new String[]{""+port});
        } catch (IOException e) {
            Debug.report(e);
        }
        return null;        
    }

}
