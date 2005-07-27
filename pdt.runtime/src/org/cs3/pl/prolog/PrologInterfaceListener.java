/*
 */
package org.cs3.pl.prolog;

import java.util.EventListener;

/**
 */
public interface PrologInterfaceListener extends EventListener{
    /**
     * @param e
     */
    void update(PrologInterfaceEvent e);
}
