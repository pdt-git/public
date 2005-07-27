/*
 */
package org.cs3.pl.prolog;

import java.util.EventListener;

/**
 * @deprecated
 */
public interface ConsultServiceListener extends EventListener {
    public void consultDataChanged(ConsultServiceEvent e);
}
