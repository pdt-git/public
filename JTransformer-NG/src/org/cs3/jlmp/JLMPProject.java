/*
 */
package org.cs3.jlmp;

import java.io.IOException;
import java.io.PrintStream;

import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.ICompilationUnit;

/**
 */
public interface JLMPProject {
    /**
     * @return the underlying project.
     */
    public IProject getProject();
    
    /**
     * @return the prolog interface for this project
     */
    public PrologInterface getPrologInterface();
    
    /**
     * stay informed about changes in the fact base.
     */
    public void addJLMPProjectListener(JLMPProjectListener l);
    
    /**
     * not longer interested in the factbase.    
     */
    public void removeJLMPProjectListener(JLMPProjectListener l);
    
    /**
     * generates the PEF representation of the given compilation unit.
     * @param cu
     * @param out
     * @throws CoreException
     * @throws IOException
     */
    public void generateFacts(ICompilationUnit cu, PrintStream out) throws IOException, CoreException;
}
