package org.cs3.pdt;

import java.util.ArrayList;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

/**
 * All kinds of string keys used by the pdt.
 */
public final  class PDT {
	
    public static final String PLUGIN_ID = "org.cs3.pdt";

    /**
     * Specifies the default level of verbosity. Valid values are "DEBUG" (VERY
     * verbose), "INFO", "WARNING","ERROR" and "NONE" (quiet)
     * 
     * The property will be read out once the Debug class is loaded, and the
     * debug level will be set accordingly. After that, the level can be changed
     * using the static Debug.setDeubgLevel(int) method.
     */
    public final static String PREF_DEBUG_LEVEL = "debug.level";

    
     
    /**
     * The basename of the resource bundle to be used by the pdt ui
     */
    
    public final static String RES_BUNDLE_UI = "org.cs3.pdt.ui";
    /**
     * log file location used by the pdt plugin.
     */
    public static final String PREF_CLIENT_LOG_FILE = "pdt.logfile";
	
	
	/**
     * @return all open IPrologProjects that operate on the given PrologInterface instance.
     * @throws CoreException
     * @deprecated we wil find a more general solution in PDT 0.2
     */
    public  static IPrologProject[] getPrologProjects(PrologInterface pif) throws CoreException{
        IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
        ArrayList l = new ArrayList();
        for (int i = 0; i < projects.length; i++) {
            IProject project = projects[i];            
            if(project.isAccessible()&&project.hasNature(PDTCore.NATURE_ID)){
                IPrologProject prologProject = (IPrologProject) project.getNature(PDTCore.NATURE_ID);
                if(prologProject.getPrologInterface()==pif){
                    l.add(prologProject);
                }
            }
        }
        IPrologProject[] r = new IPrologProject[l.size()];
        return (IPrologProject[]) l.toArray(r);
    }

}