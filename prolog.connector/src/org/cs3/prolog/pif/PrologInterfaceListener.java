/* $LICENSE_MSG$(ld) */

/*
 */
package org.cs3.prolog.pif;

import java.util.EventListener;

/**
 */
public interface PrologInterfaceListener extends EventListener{
    /**
     * @param e
     */
    void update(PrologInterfaceEvent e);

	
}

