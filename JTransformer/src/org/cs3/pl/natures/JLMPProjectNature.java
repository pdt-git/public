package org.cs3.pl.natures;
import java.io.IOException;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.builders.FactBaseBuilder;
import org.cs3.pl.builders.JLMPProjectBuilder;
import org.cs3.pl.fileops.MetaDataManagerFactory;
import org.cs3.pl.fileops.PrologMetaDataManager;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
/**
 * @see IProjectNature
 */
public class JLMPProjectNature implements IProjectNature {
	public final static String NATURE_ID = "org.cs3.pl.JTransformer.JLMPProjectNature";
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
	public FactBaseBuilder getBuilder() throws IOException {
		return PDTPlugin.getDefault().getBuilder(getProject());
	}
	
	public PrologMetaDataManager getMetaDataManager(String dir) throws IOException {
		if (dir != PrologMetaDataManager.PL && dir != PrologMetaDataManager.EXT && dir != PrologMetaDataManager.MODEL)
			return null;
		
		return MetaDataManagerFactory.getPrologMetaDataManager(PrologManager.getInstance().getHiddenClient(),dir);
	}
}