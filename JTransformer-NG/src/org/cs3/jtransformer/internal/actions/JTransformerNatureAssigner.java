package org.cs3.jtransformer.internal.actions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.internal.dialog.PrologRuntimeSelectionDialog;
import org.cs3.jtransformer.internal.dialog.RemoveJTransformerNatureDialog;
import org.cs3.jtransformer.internal.natures.JTransformerProjectNature;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
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
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import salvo.jesus.graph.DirectedAcyclicGraph;
import salvo.jesus.graph.DirectedAcyclicGraphImpl;
import salvo.jesus.graph.DirectedEdgeImpl;
import salvo.jesus.graph.Vertex;
import salvo.jesus.graph.VertexImpl;
import salvo.jesus.graph.algorithm.TopologicalSorting;

public class JTransformerNatureAssigner {
	
	public static final int STATUS_REMOVED = 0;
	public static final int STATUS_CANCELLED = 1;
	public static final int STATUS_ADDED = 0;
	private List projects;
	Hashtable vertexes;
	private boolean includeReferencedProjects = false;
	private boolean addReferenceToOutputProject = false;

	public JTransformerNatureAssigner(List projects) {
		this.projects = projects;
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
					JTransformer.CX_UNKNOWN, e);
			Debug.report(new Error(e));
			e.printStackTrace();
		}
		return STATUS_CANCELLED;

	}

	public boolean askAndRemoveNatures() throws CoreException {
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

		for (Iterator iter = projects.iterator(); iter.hasNext();) {
			IProject project = (IProject) iter.next();
			removeNature(project, dialog
					.isDeleteOutputProject(), dialog
					.isRemoveOutputProjectReference());
		}
		return true;
	}

	public boolean askAndAddNatures() throws Exception {
		Set allKeys = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry().getAllKeys();
		String factbaseName = selectAlternativePrologInterface(((IProject)projects.get(0)).getName(),allKeys);
		if(factbaseName == null) {
			return false;
		}
		List sortedProjects = topoSortProjects(projects);
		includeReferencedProjects = false;

		for (Iterator iter = sortedProjects.iterator(); iter.hasNext();) {
			IProject project = (IProject)((VertexImpl) iter.next()).getObject();
			final IJavaProject javaProject = (IJavaProject) project.getNature(JavaCore.NATURE_ID);
		    addNature(project,javaProject,factbaseName);
		}
		return true;
	}

	private List topoSortProjects(List projects) throws Exception {
		List toProcess = new ArrayList(projects);
		DirectedAcyclicGraph dag = new DirectedAcyclicGraphImpl();
		vertexes = new Hashtable();
		while(toProcess.size() > 0) {
			IProject project = (IProject)toProcess.get(0);
			toProcess.remove(project);
			Vertex vertex = getVertexForProject(toProcess,project);
			dag.add(vertex);
			final IJavaProject javaProject = (IJavaProject) project.getNature(JavaCore.NATURE_ID);
			if(javaProject == null) {
				System.err.println("DEBUG");
			}
			IClasspathEntry[] referenced = javaProject.getResolvedClasspath(true);
			for (int i = 0; i < referenced.length; i++) {
				IClasspathEntry entry = referenced[i];
				if(entry.getEntryKind() == IClasspathEntry.CPE_PROJECT) {
					if(entry.getPath().segmentCount() > 1) {
						throw new Error("NOT A PROJECT" + entry);
					}
					IProject refProject = ResourcesPlugin.getWorkspace().getRoot().getProject(entry.getPath().segment(0));
					if(refProject.getName().endsWith("-output")) {
						continue;
					}
					if(!includeReferencedProjects && !projects.contains(refProject)) {
						continue;
					}
					Vertex refVertex = getVertexForProject(toProcess,refProject);
					dag.addEdge(new DirectedEdgeImpl(vertex, refVertex));
				}
				
			}
//			for (int i = 0; i < referenced.length; i++) {
//				Vertex refVertex = getVertexForProject(referenced[i]);
//				dag.addEdge(new DirectedEdgeImpl(vertex, refVertex));
//			}
		}
		TopologicalSorting topoSort = new TopologicalSorting(dag);
		List sorted = topoSort.traverse();
		Collections.reverse(sorted);
		return sorted;
	}

	private Vertex getVertexForProject(List projects, IProject project) {
		Vertex vertex = (Vertex)vertexes.get(project);
		if(vertex != null) {
			return vertex;
		}
		if(!projects.contains(project)) {
			projects.add(project);
		}

		vertex = new VertexImpl(project);
		vertexes.put(project, vertex);
		return vertex;
	}

	/**
	 * 
	 * @param project
	 * @param javaProject
	 * @param factbaseName
	 * @return true if the natures were added
	 * @throws JavaModelException
	 * @throws CoreException
	 */
	private boolean addNature(IProject project, final IJavaProject javaProject,String factbaseName) throws JavaModelException, CoreException {
		if(javaProject == null) {
			final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			MessageDialog.openError(shell,"JTransformer", 
					"Project '" + project.getName() + "':\nYou can only assign the JTransformer nature to Java projects.");
						return false;
		}
		if(javaProject.getOption(JavaCore.COMPILER_SOURCE,true).equals(JavaCore.VERSION_1_5)) {
			final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			MessageDialog.openError(shell,"JTransformer", 
					"Project '" + project.getName() + "':\nJTransformer is not yet compatible with Java 5.0 source code.\n" +
					"Please change the source compatibility to 1.4 in the project preferences.");
						return false;
		}
		
			IProject destProject = CreateOutdirUtils.getInstance().createOutputProject(project);
	    	project.setPersistentProperty(new QualifiedName("", JTransformer.PROLOG_RUNTIME_KEY),factbaseName);

			JTransformerPlugin.getDefault().setPreferenceValue(project,JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_ACTIVATED);

		    addJTransformerNature(project);

			destProject.refreshLocal(IResource.DEPTH_INFINITE, null);
		    //action.setChecked(true);

		    if(addReferenceToOutputProject) {
		    	JTUtils.addReferenceToOutputProjectIfNecessary(javaProject, destProject);
		    }
		    return true;
	}

	private void removeNature(IProject project, boolean deleteOutputProject, boolean removeOutputProjectReference) throws CoreException {

		JTUtils.getNature(project).clearAllMarkersWithJTransformerFlag();
		JTransformerProjectNature.removeJTransformerNature(project);
		final IProject outputProject = JTUtils.getOutputProject(project);
		if(removeOutputProjectReference) {
			removeOutputProjectReference(project, outputProject);
		}
		if(deleteOutputProject) {
		    deleteOutputProject(project,outputProject);					
		}
		//action.setChecked(false);
		JTransformerPlugin.getDefault().setPreferenceValue(project,JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_DISABLED);
	}

	private void removeOutputProjectReference(final IProject project, final IProject outputProject) {
		Job j = new Job("Building workspace") {
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
		Job j = new Job("Building workspace") {
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

	private String selectAlternativePrologInterface(String defaultName, Set allKeys) throws CoreException {
//		if(allKeys.size() > 0) {
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
