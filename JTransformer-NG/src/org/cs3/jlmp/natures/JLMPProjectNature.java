package org.cs3.jlmp.natures;
import java.util.List;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPPlugin;
import org.cs3.jlmp.builders.JLMPProjectBuilder;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;

/**
 * @see IProjectNature
 */
public class JLMPProjectNature implements IProjectNature {
	public final static String NATURE_ID = "org.cs3.jlmp.JLMPProjectNature";
	//The project for which this nature was requested,
	//see IProject.getNature(String)
	private IProject project;
	/**
	 *  
	 */
	public JLMPProjectNature() {
	}
	/**
	 * @see IProjectNature#configure
	 */
	public void configure() throws CoreException {
		Debug.debug("configure was called");
		IProjectDescription descr = project.getDescription();
		ICommand sheepBuilder = descr.newCommand();
		sheepBuilder.setBuilderName(JLMPProjectBuilder.BUILDER_ID);
		ICommand builders[] = descr.getBuildSpec();
		for (int i = 0; i < builders.length; i++) {
			if (builders[i].getBuilderName().equals(
					JLMPProjectBuilder.BUILDER_ID)) {
				return;
			}
		}
		ICommand newBuilders[] = new ICommand[builders.length + 1];
		System.arraycopy(builders, 0, newBuilders, 0, builders.length);
		newBuilders[builders.length] = sheepBuilder;
		descr.setBuildSpec(newBuilders);
		project.setDescription(descr, null);
	}
	/**
	 * @see IProjectNature#deconfigure
	 */
	public void deconfigure() throws CoreException {
		IProjectDescription descr = project.getProject().getDescription();
		Debug.debug("deconfigure was called");
		ICommand builders[] = descr.getBuildSpec();
		int index = -1;
		for (int i = 0; i < builders.length; i++) {
			if (builders[i].getBuilderName().equals(
					JLMPProjectBuilder.BUILDER_ID)) {
				index = i;
				break;
			}
		}
		if (index != -1) {
			ICommand newBuilders[] = new ICommand[builders.length - 1];
			System.arraycopy(builders, 0, newBuilders, 0, index);
			System.arraycopy(builders, index + 1, newBuilders, index,
					builders.length - index - 1);
			descr.setBuildSpec(newBuilders);
		}
	}
	/**
	 * @see IProjectNature#getProject
	 */
	public IProject getProject() {
		return project;
	}
	/**
	 * @see IProjectNature#setProject
	 */
	public void setProject(IProject project) {
		this.project = project;
	}
   
	public PrologInterface getPrologInterface(){
	    PrologInterface pif = PDTPlugin.getDefault().getPrologInterface(getProject().getName());

	    
//	    List libraries = pif.getBootstrapLibraries();
//	    ResourceFileLocator l = JLMPPlugin.getDefault().getResourceLocator(JLMP.ENGINE);
//	    String name = Util.prologFileName(l.resolve(JLMP.MAIN_PL));
//	    if(!libraries.contains(name)){
//	        libraries.add(name);
//	        if(pif.isUp());
//	        PrologSession s = pif.getSession();
//	        s.queryOnce("['"+name+"']");
//	    }
        return pif;
	}
	
	
	
	
}