/*
 */
package org.cs3.pl.model.internal.runtime;

import org.cs3.pl.model.IPrologElement;
import org.cs3.pl.model.IString;
import org.cs3.pl.model.PLRuntime;


/**
 */
public class PLString extends AbstractTerm implements IString {

    /**
     * @param runtime
     * @param parent
     * @param string
     */
    public PLString(PLRuntime runtime, IPrologElement parent, String string) {
        super(runtime,parent,string);
    }

}
