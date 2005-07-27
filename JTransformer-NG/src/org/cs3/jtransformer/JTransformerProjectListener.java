package org.cs3.jtransformer;

import java.util.EventListener;

/**
 * get notified when a jtransformer project's PEFs are updated.  
 */
public interface JTransformerProjectListener extends EventListener{
    public void factBaseUpdated(JTransformerProjcetEvent e);
}
