/*
 */
package org.cs3.jtransformer;

import java.io.IOException;

import org.cs3.jtransformer.regenerator.ISourceRegenerator;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.OptionProvider;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.ICompilationUnit;

/**
 * A Java Logical Metaprogramming Project.
 * <p>
 * This is the main entry point into the JTransformer api for most clients. Similar to
 * IJavaProjects, instances can be obtained from any <code>IProject</code>
 * instance with the JTransformer Project Nature (<code>JTransformer.NATURE_ID</code>):<br>
 * <code>
 * <pre>
 * 
 *     IProject project = .....
 *     JTransformerProject jtransformerProject = (JTransformerProject)project.getNature(JTransformer.NATURE_ID);
 *  
 * </pre>
 * </code><br>
 * A project with the JTransformer Nature will maintain a logical PEF representation of
 * its Java source code. This representation is brought up to date after any
 * change to the underlying workspace reosurces. Clients can register a
 * JTransformerProjectListener to get notified when the factbase is updated. To interact
 * with the prolog system that contains the PEF data for the project, the client
 * can obtain a PrologInterface instance from the respective JTransformerProject.
 *  
 */
public interface JTransformerProject extends OptionProvider{
    /**
     * @return the underlying project.
     */
    public IProject getProject();

    /**
     * obtain the projects PrologInterface.
     * <br>
     * <b>NOTE: </b>The API somewhat suggests that each JTransformerProject has its own
     * exclusive PrologInterface (and thus a Prolog knowledge base) on its own.
     * In fact, that is planned for the future. Right now however, all Projects
     * share the same PrologInterface.
     * 
     * @return the prolog interface for this project
     */
    public PrologInterface getPrologInterface();

    /**
     * stay informed about changes in the fact base.
     */
    public void addJTransformerProjectListener(JTransformerProjectListener l);

    /**
     * not longer interested in the factbase.
     */
    public void removeJTransformerProjectListener(JTransformerProjectListener l);

    /**
     * generates the PEF representation of the given compilation unit.
     * 
     * @param cu
     * @param out
     * @throws CoreException
     * @throws IOException
     * @deprecated this is only used in some irrelevant test cases. I would like
     *                   to get rid of it.
     */
    public void generateFacts(AsyncPrologSession session, ICompilationUnit cu)
            throws IOException, CoreException, PrologInterfaceException;

    /**
     * 
     * @return an array of objects describing the configurable options for this
     * JTransformer project.
     */
    public Option[] getOptions();
    
//    /**
//     * look up a preference value.
//     * <p>
//     * tries the following values in the given order and returns the first
//     * non-null result. If everything returns null, the given defaultValue is
//     * returned.
//     * <ul>
//     * <li>System.getProperty(key)</li>
//     * <li>getProject().getPersistentProperty(key)</li>
//     * <li>if an option with the given id exists in the array returned by
//     * getOptions(), take its default value</li>
//     * <li>the given default value
//     * </ul>
//     * 
//     * @param key
//     * @return the value or specified default if no such key exists..
//     * @throws CoreException
//     * @throws CoreException
//     */
//    public String getPreferenceValue(String key, String defaultValue) ;

    /**
     * 
     */
    public void reconfigure();

    /**
     * @return a source code regenerator for this project.
     */
    public ISourceRegenerator getSourceRegenerator();
    
}
