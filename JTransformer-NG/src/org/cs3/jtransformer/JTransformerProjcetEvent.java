package org.cs3.jtransformer;

import java.util.EventObject;


public class JTransformerProjcetEvent extends EventObject {

    /**
     * Comment for <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;

    public JTransformerProjcetEvent(Object source) {
        super(source);
    }
    
    public JTransformerProject getJTransformerroject(){
        return getSource() instanceof JTransformerProject ?(JTransformerProject) source :null;
    }

}
