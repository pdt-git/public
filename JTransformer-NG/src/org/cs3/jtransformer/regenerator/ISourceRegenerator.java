package org.cs3.jtransformer.regenerator;

import org.cs3.pl.prolog.PrologInterfaceException;


public interface ISourceRegenerator {

    public void generateDirtyClasses() throws PrologInterfaceException;
    public IAffectedFile[] getAffectedFiles() throws PrologInterfaceException;

} 