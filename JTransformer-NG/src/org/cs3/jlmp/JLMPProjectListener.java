package org.cs3.jlmp;

import java.util.EventListener;

/**
 * get notified when a jlmp project's PEFs are updated.  
 */
public interface JLMPProjectListener extends EventListener{
    public void factBaseUpdated(JLMPProjectEvent e);
}
