/*
 */
package org.cs3.pl.model.internal.runtime;

import org.cs3.pl.model.IAtom;
import org.cs3.pl.model.IPrologElement;
import org.cs3.pl.model.PLRuntime;

/**
 */
public class Atom extends AbstractTerm implements IAtom {

   

    /**
     * @param runtime
     * @param parent
     * @param string
     */
    public Atom(PLRuntime runtime, IPrologElement parent, String string) {
        super(runtime,parent,string);        
    }

}
