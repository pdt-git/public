/*
 */
package org.cs3.pl.model.internal.runtime;

import org.cs3.pl.model.IPrologElement;
import org.cs3.pl.model.IVariable;
import org.cs3.pl.model.PLRuntime;

/**
 */
public class Variable extends AbstractTerm implements IVariable {

    /**
     * @param runtime
     * @param parent
     * @param string
     */
    public Variable(PLRuntime runtime, IPrologElement parent, String string) {
        super(runtime,parent,string);
        
    }

}
