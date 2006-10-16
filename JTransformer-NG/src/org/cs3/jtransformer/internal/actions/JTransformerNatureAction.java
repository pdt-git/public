package org.cs3.jtransformer.internal.actions;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.JTransformerProject;
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
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
/**
 * Called by Eclipse to implement real-time updates and building 
 * of JTransformer-Projects.
 * 
 * @inheritDoc
 */
public class JTransformerNatureAction implements IObjectActionDelegate {
	public final static String ACTION_ID = "org.cs3.pl.JTransformer.JTransformerNatureAction";
	private IProject project;
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
	 *           org.eclipse.ui.IWorkbenchPart)
	 */
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {
		if (project == null){
			action.setChecked(false);
			return;
		}
		try {
			IProjectDescription ipd = project.getDescription();
	        final IJavaProject javaProject = (IJavaProject) project.getNature(JavaCore.NATURE_ID);
			
			if (ipd.hasNature(JTransformer.NATURE_ID)) {

				RemoveJTransformerNatureDialog dialog = new RemoveJTransformerNatureDialog(
			    		UIUtils.getDisplay().getActiveShell());
			    boolean ok = dialog.open();
			    
			    if(!ok) {
			    	return;
			    }
			    
			    
				JTransformerProjectNature.removeJTransformerNature(project);
				final IProject outputProject = JTUtils.getOutputProject(project);
				if(dialog.isRemoveOutputProjectReference()) {
					removeOutputProjectReference(javaProject, outputProject);
				}
				if(dialog.isDeleteOutputProject()) {
		            deleteOutputProject(outputProject);					
				}
				action.setChecked(false);
				JTransformerPlugin.getDefault().setPreferenceValue(project,JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_DISABLED);

			} else {
			    //removeNatureFromAllOtherProjects();
			    //PrologManager.getInstance().restart();
				
				//javaProject.set
				
		        String[] requiredProjects = javaProject.getRequiredProjectNames();
		        
		        if(javaProject == null) {
		    		final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
					MessageDialog.openError(shell,"JTransformer", 
							"You can only assign the JTransformer nature to Java projects.");
					return;
		        }
		        if(javaProject.getOption(JavaCore.COMPILER_SOURCE,true).equals(JavaCore.VERSION_1_5)) {
		    		final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
					MessageDialog.openError(shell,"JTransformer", 
							"JTransformer is not yet compatible with Java 5.0 source code.\n" +
							"Please change the source compatibility to 1.4 in the project preferences.");
					return;
		        }
		        
		        
				/*
				 *  Mark Schmatz:
				 *  Moved from LogicAJPlugin and modified
				 */
			    Set allKeys = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry().getAllKeys();
			    boolean notCanceled = selectAlternativePrologInterface(allKeys);
			    if (notCanceled) {
					IProject destProject = CreateOutdirUtils.getInstance().createOutputProject(project);
			    	JTransformerPlugin.getDefault().setPreferenceValue(JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_IN_PROCESS);

				    addJTransformerNature(project);
	//			    JTransformerProjectNature jtNature = (JTransformerProjectNature)project.getNature(JTransformer.NATURE_ID);
	//			    	jtNature.setPreferenceValue(JTransformer.PROLOG_RUNTIME_KEY, key);
	//			    }
	
					destProject.refreshLocal(IResource.DEPTH_INFINITE, null);
				    action.setChecked(true);
	
					JTUtils.addReferenceToOutputProjectIfNecessary(javaProject, destProject);
					
			    }
			    
			    
			}
//			action.setChecked(project.getDescription().hasNature(JTransformer.NATURE_ID));
		} catch (CoreException e) {
			Debug.report(e);
		}
	}


	private void removeOutputProjectReference(final IJavaProject javaProject, final IProject outputProject) {
		Job j = new Job("Building workspace") {
		    public IStatus run(IProgressMonitor monitor) {
				try {
					JTUtils.removeReferenceToOutputProjectIfNecessary(javaProject, outputProject,monitor);
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
	
	private void deleteOutputProject(final IProject outputProject) {
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


	private boolean selectAlternativePrologInterface(Set allKeys) throws CoreException {
		if(allKeys.size() > 0) {
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
		    		UIUtils.getDisplay().getActiveShell(),keyList,subscriptionsList,project.getName());
		    String key = dialog.open();
		    
		    if(key != null) {
		    	project.setPersistentProperty(new QualifiedName("", JTransformer.PROLOG_RUNTIME_KEY),key);
		    	return true;
		    }
		    return false;
		}
		return true;
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

    /**
     * @param project2
     * @throws CoreException
     */
    private void removeNatureFromAllOtherProjects() throws CoreException {
        IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
        for (int i = 0; i < projects.length; i++)
            if(projects[i].isOpen()) 
                JTransformerProjectNature.removeJTransformerNature(projects[i]);
    }

    /* TODO CLEAN UP THIS METHOD! von ld, fuer ld :-)
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
	 *           org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection)
	{
		project=null;
		if (selection instanceof IStructuredSelection) {
			Object obj = ((IStructuredSelection) selection).getFirstElement();
			if (obj instanceof IProject) {
				// the plugin.xml file should make sure it is indeed
				// a java project
				project = (IProject) obj;
							
			} 
			else if (obj instanceof IAdaptable) {
				IAdaptable a = (IAdaptable) obj;
				IResource r = (IResource) a.getAdapter(IResource.class);
				if (r != null && IResource.PROJECT == r.getType()) {
					project = (IProject) r;
				}		
			} 			
		} 
		if(project!=null){
			if (project.isOpen()) {
				action.setEnabled(true);
				try {
					boolean checked = project.getDescription().hasNature(JTransformer.NATURE_ID);
					action.setChecked(checked);
				} catch (CoreException e) {
					Debug.report(e);
				}
			}
			else{
				action.setEnabled(false);
			}
		}
	}
	

}