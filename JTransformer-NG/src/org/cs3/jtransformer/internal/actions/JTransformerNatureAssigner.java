package org.cs3.jtransformer.internal.actions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.cs3.jtransformer.JTDebug;
import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.internal.dialog.PrologRuntimeSelectionDialog;
import org.cs3.jtransformer.internal.dialog.RemoveJTransformerNatureDialog;
import org.cs3.jtransformer.internal.natures.JTransformerNature;
import org.cs3.jtransformer.internal.natures.JTransformerSubscription;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.prolog.PrologInterfaceException;
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
	
		String factbaseName = selectAlternativePrologInterface(((IProject)projects.get(0)).getName());
		if(factbaseName == null) {
			return false;
		}
		TopoSortProjects sorter = new TopoSortProjects();
		List sortedProjects = sorter.sort(includeReferencedProjects,projects);
		includeReferencedProjects = false;
		
		for (Iterator iter = sortedProjects.iterator(); iter.hasNext();) {
		    addNature((IProject)iter.next(), factbaseName);
		}
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

		if(javaProject == null) {
			final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			MessageDialog.openError(shell,"JTransformer", 
					"Project '" + project.getName() + "':\nYou can only assign the JTransformer nature to Java projects.");
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
			JTransformerPlugin.getDefault().setNonPersistantPreferenceValue(project,JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_ACTIVATED);
			
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

	public void removeNature(IProject project, boolean deleteOutputProject, boolean removeOutputProjectReference) throws CoreException, PrologInterfaceException {

		JTUtils.clearAllMarkersWithJTransformerFlag(project);
		JTransformerNature.removeJTransformerNature(project);
		JTDebug.info("JTransformerNatureAssigner.removeNature: called clearing persistant factbase");
		JTUtils.clearPersistantFacts(JTUtils.getFactbaseKeyForProject(project));
		final IProject outputProject = JTUtils.getOutputProject(project);
		if(removeOutputProjectReference) {
			removeOutputProjectReference(project, outputProject);
		}
		if(deleteOutputProject) {
		    deleteOutputProject(project,outputProject);					
		}
		//action.setChecked(false);
		JTransformerPlugin.getDefault().setNonPersistantPreferenceValue(project,JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_DISABLED);
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




}
