package org.cs3.jlmp;

import java.util.EventObject;


public class JLMPProjectEvent extends EventObject {

    /**
     * Comment for <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;

    public JLMPProjectEvent(Object source) {
        super(source);
    }
    
    public JLMPProject getJLMProject(){
        return getSource() instanceof JLMPProject ?(JLMPProject) source :null;
    }

}
