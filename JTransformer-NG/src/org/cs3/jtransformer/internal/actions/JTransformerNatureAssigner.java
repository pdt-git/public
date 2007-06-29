package org.cs3.jtransformer.internal.actions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.cs3.jtransformer.JTDebug;
import org.cs3.jtransformer.JTPrologFacade;
import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.internal.dialog.PrologRuntimeSelectionDialog;
import org.cs3.jtransformer.internal.dialog.RemoveJTransformerNatureDialog;
import org.cs3.jtransformer.internal.natures.JTransformerNature;
import org.cs3.jtransformer.internal.natures.JTransformerSubscription;
import org.cs3.jtransformer.internal.natures.TimeMeasurement;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

public class JTransformerNatureAssigner {


	public static final int STATUS_REMOVED = 0;
	public static final int STATUS_CANCELLED = 1;
	public static final int STATUS_ADDED = 0;
	private List projects;
	private boolean includeReferencedProjects = false;
	private boolean addReferenceToOutputProject = false;
	private TimeMeasurement timeAssignNatures;

	public JTransformerNatureAssigner(List projects) {
		this.projects = projects;
	}

	public JTransformerNatureAssigner(IProject project) {
		this.projects = new ArrayList();
		projects.add(project);
	}

	/**
	 * 
	 * @return STATUS_CANCELLED, STATUS_ADDED or STATUS_REMOVED
	 */
	public int toggleJTransformerNatureOnAllProjects() {
		try {
			if (!JTUtils.allProjectsHaveJTNature(projects)) {
				if(askAndAddNatures()){
					return STATUS_ADDED;
				} else {
					return STATUS_CANCELLED;
				}

			} else {
				if(askAndRemoveNatures()){
					return STATUS_REMOVED;
				} else {
					return STATUS_CANCELLED;
				}
			}
		} catch (Exception e) {
			UIUtils.logAndDisplayError(JTransformerPlugin.getDefault()
					.getErrorMessageProvider(), UIUtils.getDisplay()
					.getActiveShell(), JTransformer.ERR_UNKNOWN,
					JTransformer.ERR_CONTEXT_EXCEPTION, e);
			JTDebug.report(new Error(e));
			e.printStackTrace();
			// reset to previous state:
			JTUtils.setAutoBuilding(true);
		}
		return STATUS_CANCELLED;

	}

	public boolean askAndRemoveNatures() throws CoreException, PrologInterfaceException {
		String projectNames;
		if(projects.size() == 1) {
			projectNames = "project " + ((IProject) projects.get(0)).getName();
		} else {
			projectNames = "all selected projects";
		}
		RemoveJTransformerNatureDialog dialog = new RemoveJTransformerNatureDialog(
				UIUtils.getDisplay().getActiveShell(),projectNames);
		boolean ok = dialog.open();
		if (!ok) {
			return false;
		}
		removeNatures(dialog);
		return true;
	}

	private void removeNatures(RemoveJTransformerNatureDialog dialog) throws CoreException, PrologInterfaceException {
		if(projects.size() > 0){
			JTUtils.clearPersistentFacts(JTUtils.getFactbaseKeyForProject((IProject)projects.get(0)));
		}
		for (Iterator iter = projects.iterator(); iter.hasNext();) {
			IProject project = (IProject) iter.next();
			try {
				removeNature(project, dialog
						.isDeleteOutputProject(), dialog
						.isRemoveOutputProjectReference());
			} catch(CoreException core) {
				core.printStackTrace();
				final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
				MessageDialog.openError(shell,"JTransformer", 
						"Could not remove the JTransformer nature for project'" + project.getName() +
						"' because the following exception occurred:\n" + core.getLocalizedMessage());

			}
		}
	}

	public boolean askAndAddNatures() throws Exception {
	
		if(!autoBuildTurnedOn()) {
			final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			MessageDialog.openError(shell,"JTransformer", 
					"Auto-build is not turned on. JTransformer will not work properly when auto-build is turned off.\n" +
					"Please change this setting via the menu item \"Project->Build Automatically\" an try again.");
						return false;
		}

		String factbaseName = selectAlternativePrologInterface(((IProject)projects.get(0)).getName());
		if(factbaseName == null) {
			return false;
		}
		
		JTUtils.clearPersistentFacts(factbaseName);

		// Auto building is deactivated while the nature is assigned to avoid unexpected build runs
		// FIXME: I do not fully understand the consequences in case that the project is not in sync with the file system.  
		JTUtils.setAutoBuilding(false);

		timeAssignNatures = new TimeMeasurement("Assign nature(s) for factbase: " + factbaseName,JTDebug.LEVEL_INFO);

		TopoSortProjects sorter = new TopoSortProjects();
		List sortedProjects = sorter.sort(includeReferencedProjects,projects);
		includeReferencedProjects = false;
		
		for (Iterator iter = sortedProjects.iterator(); iter.hasNext();) {
			JTDebug.info("JTransformerNatureAssigner.askAndAddNatures: " + (IProject)iter.next() + ", factbase: " + factbaseName);
		}

		for (Iterator iter = sortedProjects.iterator(); iter.hasNext();) {
		    addNature((IProject)iter.next(), factbaseName);
		}
		timeAssignNatures.logTimeDiff();
		JTransformerPlugin.getNature((IProject)projects.get(0)).getPrologInterface().start();
		return true;
	}





	/**
	 * 
	 * @param project
	 * @param factbaseName
	 * @return true if the natures were added
	 * @throws JavaModelException
	 * @throws CoreException
	 */
	public boolean addNature(IProject project, String factbaseName) throws JavaModelException, CoreException {
		final IJavaProject javaProject = (IJavaProject) project.getNature(JavaCore.NATURE_ID);
		JTDebug.info("JTransformerNatureAssigner.addNature to project: " + project + ", factbase: " + factbaseName);

		if(javaProject == null) {
			final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			MessageDialog.openError(shell,"JTransformer", 
					"Project '" + project.getName() + "':\nYou can assign the JTransformer nature only to Java projects.");
						return false;
		}

			 
//		if(javaProject.getOption(JavaCore.COMPILER_SOURCE,true).equals(JavaCore.VERSION_1_5)) {
//			final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
//			MessageDialog.openError(shell,"JTransformer", 
//					"Project '" + project.getName() + "':\nJTransformer is not yet compatible with Java # source code.\n" +
//					"Please change the source compatibility to 1.4 in the project preferences.");
//						return false;
//		}
			IProject destProject = CreateOutdirUtils.getInstance().createOutputProject(project);
	    	project.setPersistentProperty(new QualifiedName("", JTransformer.PROLOG_RUNTIME_KEY),factbaseName);
			JTransformerPlugin.getDefault().setNonPersistentPreferenceValue(project,JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_ACTIVATED);
			
//			final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
//			
//			List sharingProjects = new ArrayList();
//			for (int i = 0; i < projects.length; i++) {
//				if(projects[i].isOpen()){
//					String runtimeKey = JTransformerPlugin.getDefault().getPreferenceValue(projects[i], 
//							JTransformer.PROLOG_RUNTIME_KEY, null);
//					if(runtimeKey != null && runtimeKey.equals(factbaseName) &&
//					   projects[i].hasNature(JTransformer.NATURE_ID) ){
//						sharingProjects.add(projects[i]);
//					}
//				}
//			}

//			if(sharingProjects.size() > 0 && 
//			   JTransformerPlugin.getNature(((IProject)sharingProjects.get(0))).getPrologInterface().isDown())
			
			JTransformerSubscription subcription = JTransformerPlugin.getJTransformerSubscription(factbaseName);
			if(subcription != null && subcription.isPrologInterfaceUp())
			{
		    	JTransformerPlugin.getDefault().setIgnoreThisBuild(project);
			}
			
		    addJTransformerNature(project);

			destProject.refreshLocal(IResource.DEPTH_INFINITE, null);
		    //action.setChecked(true);

		    if(addReferenceToOutputProject) {
		    	JTUtils.addReferenceToOutputProjectIfNecessary(javaProject, destProject);
		    }
		    return true;
	}

	private boolean autoBuildTurnedOn() {
		 String key = ResourcesPlugin.PREF_AUTO_BUILDING;
		 return ResourcesPlugin.getPlugin().getPluginPreferences().getBoolean(key);
	}

	public void removeNature(IProject project, boolean deleteOutputProject, boolean removeOutputProjectReference) throws CoreException, PrologInterfaceException {

		JTUtils.clearAllMarkersWithJTransformerFlag(project);
		JTransformerNature.removeJTransformerNature(project);
		JTDebug.info("JTransformerNatureAssigner.removeNature: called clearing persistent factbase");
		JTUtils.clearPersistentFacts(JTUtils.getFactbaseKeyForProject(project));
		final IProject outputProject = JTUtils.getOutputProject(project);
		if(removeOutputProjectReference) {
			removeOutputProjectReference(project, outputProject);
		}
		if(deleteOutputProject) {
		    deleteOutputProject(project,outputProject);					
		}
		//action.setChecked(false);
		JTransformerPlugin.getDefault().setNonPersistentPreferenceValue(project,JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_DISABLED);
	}

	private void removeOutputProjectReference(final IProject project, final IProject outputProject) {
		Job j = new Job("Removing Reference to JTransformer Output Project.") {
		    public IStatus run(IProgressMonitor monitor) {
				try {
					JTUtils.removeReferenceToOutputProjectIfNecessary(
							(IJavaProject) project.getNature(JavaCore.NATURE_ID), 
							 outputProject,
							 monitor);
				} catch (CoreException e) {
					return new Status(
							Status.ERROR,JTransformer.PLUGIN_ID,Status.OK,"could not delete output project of project " + project.getName(),e);
				}
		        return Status.OK_STATUS;
		    }
		      public boolean belongsTo(Object family) {
			         return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
		      }
		};
		j.setRule(ResourcesPlugin.getWorkspace().getRoot());
		j.schedule();
	}
	
	private void deleteOutputProject(final IProject project, final IProject outputProject) {
		Job j = new Job("Deleting JTransformer Output Project.") {
		    public IStatus run(IProgressMonitor monitor) {
				try {
					outputProject.delete(true, monitor);
				} catch (CoreException e) {
					return new Status(
							Status.ERROR,JTransformer.PLUGIN_ID,Status.OK,"could not delete output project of project " + project.getName(),e);
				}
		        return Status.OK_STATUS;
		    }
		      public boolean belongsTo(Object family) {
			         return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
		      }
		};
		j.setRule(ResourcesPlugin.getWorkspace().getRoot());
		j.schedule();
	}

	/**
	 * Auxiliary method that can be used to
	 * selet a prolog interface key based on the existing
	 * prolog interface registry.
	 * 
	 * @param defaultName
	 * @return
	 * @throws CoreException
	 */
	public String selectAlternativePrologInterface(String defaultName) throws CoreException {
//		if(allKeys.size() > 0) {
		Set allKeys = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry().getAllKeys();
		    List keyList = new ArrayList();
		    List subscriptionsList = new ArrayList();
		    for (Iterator iter = allKeys.iterator(); iter.hasNext();) {
		    	String key = (String)iter.next();
				keyList.add(key);
				Set subscriptions = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry().getSubscriptionsForPif(key);
				StringBuffer subscriptionsString = new StringBuffer();
				for (Iterator iterator = subscriptions.iterator(); iterator
						.hasNext();) {
					Subscription element = (Subscription) iterator.next();
					if(subscriptionsString.length() > 0) {
						subscriptionsString.append(",");
					}
					subscriptionsString.append(element.getName());
				}
				
				subscriptionsList.add(subscriptionsString.toString());
			}
			PrologRuntimeSelectionDialog dialog = new PrologRuntimeSelectionDialog(
		    		UIUtils.getDisplay().getActiveShell(),keyList,subscriptionsList,defaultName);
		    String key = dialog.open();
		    
		    if(key.length() > 0) {
		    	//project.setPersistentProperty(new QualifiedName("", JTransformer.PROLOG_RUNTIME_KEY),key);
		    	includeReferencedProjects = dialog.isIncludeReferencedProjects();
		    	addReferenceToOutputProject = dialog.isAddReferenceToOutputProject();
		    	return key;
		    } else {
		    	includeReferencedProjects = false;
		    	addReferenceToOutputProject = false;
		    }
		    return null;
//		}
//		return defaultName;
	}

	/**
     * @param ipd
     * @return
	 * @throws CoreException
     */
    private void addJTransformerNature(IProject project) throws CoreException {
        IProjectDescription ipd = project.getDescription();
        String[] oldNIDs = ipd.getNatureIds();
        String[] newNIDs = new String[oldNIDs.length + 1];
        newNIDs[0] = JTransformer.NATURE_ID;
        System.arraycopy(oldNIDs, 0, newNIDs, 1, oldNIDs.length);
		ipd.setNatureIds(newNIDs);
		  if(!project.isSynchronized(IResource.DEPTH_ONE)){
              project.refreshLocal(IResource.DEPTH_ONE,null);
          }
		project.setDescription(ipd, null);
    }

	static public void removeAllNatures() throws CoreException, PrologInterfaceException {
		IProject[] allProjects = ResourcesPlugin.getWorkspace().getRoot()
				.getProjects();
		List projectsWithNature = new ArrayList();
		for (int i = 0; i < allProjects.length; i++) {
				if (allProjects[i].isOpen()
						&& allProjects[i].hasNature(JTransformer.NATURE_ID)) {
					projectsWithNature.add(allProjects[i]);
					String key = JTransformerPlugin.getDefault().getPreferenceValue(allProjects[i], JTransformer.PROLOG_RUNTIME_KEY, null);
					if(key != null){
						PrologInterface pif = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry().getPrologInterface(key);
						if(pif != null && pif.isUp()){
							JTUtils.queryOnceSimple(pif,JTPrologFacade.CLEAR_TREE_FACTBASE);
						}
					}
					
				}
		}

		if (projectsWithNature.size() == 0) {
			return;
		}
		JTransformerNatureAssigner assigner = new JTransformerNatureAssigner(projectsWithNature);
		assigner.askAndRemoveNatures();
		
		
	}


}
