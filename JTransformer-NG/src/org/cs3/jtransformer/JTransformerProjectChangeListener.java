package org.cs3.jtransformer;

import java.util.ArrayList;
import java.util.HashMap;

import org.cs3.jtransformer.internal.actions.JTransformerNatureAssigner;
import org.cs3.jtransformer.internal.natures.JTransformerNature;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

public class JTransformerProjectChangeListener implements
		IResourceChangeListener {

	/**
	 * @see IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
	 */
	public void resourceChanged(IResourceChangeEvent event) {

		IResourceDelta root = event.getDelta();

		// Get all the affected Children. One of them would be the newly

		// created project
		if (root != null) {
			IResourceDelta[] projectDeltas = root.getAffectedChildren();

			for (int i = 0; i < projectDeltas.length; i++) {

				// Get individual delta's

				IResourceDelta delta = projectDeltas[i];

				IResource resource = delta.getResource();

				if (resource instanceof IProject) {
					final IProject project = (IProject) resource;
					try {
						if (delta.getFlags() == IResourceDelta.OPEN) {
							if (!project.isOpen()) {
								removeNatureIfAssigned(project);
								closeOutputProject(project);
							} else {
								onOpenProject(project);
							}
						} else if (delta.getKind() == IResourceDelta.REMOVED) {
							removeNatureIfAssigned(project);
							deleteOutputProject(project);
							

						}
					} catch (CoreException e) {
						 JTUtils.logAndDisplayUnknownError(e);
					}
				} else if(resource instanceof IWorkspace )
				{
					System.err.println("WORKSPACE CHANGED: " + delta.getFlags() + ", " + delta.getKind());
				}

			}
		} 
	}

	private void onOpenProject(final IProject project) throws CoreException {
		JTransformerPlugin.getDefault().setIgnoreThisBuild(project);
		
		if (project.hasNature(JTransformer.NATURE_ID)) {
			if(JTransformerPlugin.getDefault().getPreferenceValue(project, JTransformer.PROLOG_RUNTIME_KEY, null) == null)
			{
				JTransformerPlugin.getDefault().setIgnoreThisBuild(project);
				
				// assigner.removeNature must not be execute on this thread. The UI thread works fine - I have no idea why this works
				// especially because it is called synchronized.
				// Probably this is related to the import view used to import this project.
				UIUtils.getDisplay().syncExec(new Runnable() {
					public void run() {
						try {
							final JTransformerNatureAssigner assigner = new JTransformerNatureAssigner(project);
							assigner.removeNature(project, true, false);
						} catch(Exception ex) {
							JTDebug.report(ex);
						}
					}
				});
				
//				String name = assigner.selectAlternativePrologInterface(project.getName());
//				JTransformerPlugin.getDefault().setPreferenceValue(project, JTransformer.PROLOG_RUNTIME_KEY, name);
				
				// forking, because tree modifications not allowed on this thread
//				Thread thread = new Thread() {
//					public void run() {
//						try {
//							removeNatureIfAssigned(project);
//							final JTransformerNatureAssigner assigner = new JTransformerNatureAssigner(project);
//							assigner.removeNature(project, true, false);
//							UIUtils.getDisplay().asyncExec(new Runnable() {
//								public void run() {
//									try {
//										assigner.askAndAddNatures();
//									} catch(Exception ex) {
//										JTDebug.report(ex);
//									}
//								}
//								
//							});
//						} catch (Exception e) {
//							JTDebug.report(e);
//						}
//					}
//				};
//				thread.start();
//				
			} else {
				openOutputProject(project);
				if(JTransformerPlugin.getNature(project).getPrologInterface().isUp())
				{
					buildProject(project);
				}
			}
		}
	}

	private Job buildProject(final IProject project) {
		Job j = new Job("Building JTransformer PEFs for project " + project.getName()) {
			public IStatus run(IProgressMonitor monitor) {
				try {
					project.build(IncrementalProjectBuilder.FULL_BUILD, 
							JTransformer.BUILDER_ID,
							new HashMap(),
							monitor);
				} catch (OperationCanceledException opc) {
					return Status.CANCEL_STATUS;
				} catch (Exception e) {
					return new Status(IStatus.ERROR,
							JTransformer.PLUGIN_ID, -1,
							"Problems during build", e);
				}
				return Status.OK_STATUS;
			}
			public boolean belongsTo(Object family) {
				return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
			}
		};
			j.setRule(ResourcesPlugin.getWorkspace().getRoot());
			j.schedule();
		return j;
	}

	private void deleteOutputProject(IProject project) throws CoreException {
		
		final IProject outputProject = JTUtils.getOutputProject(project);
		if(outputProject.exists()){
			syncWithWorkspace(new WorkspaceJob("deleting project " + outputProject.getName()) {
				public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
					outputProject.delete(false, monitor);
					return Status.OK_STATUS;
				}
			});
		}

	}

	private void closeOutputProject(IProject project) throws CoreException {
		final IProject outputProject = JTUtils.getOutputProject(project);
		if(outputProject.exists() && outputProject.isOpen()){
			syncWithWorkspace(new WorkspaceJob("closing project " + outputProject.getName()) {
				public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
					outputProject.close(monitor);
					return Status.OK_STATUS;
				}
			});
		}
		
	}

	private void openOutputProject(IProject project) throws CoreException {
		final IProject outputProject = JTUtils.getOutputProject(project);
		if(outputProject.exists() && !outputProject.isOpen()){
			syncWithWorkspace(new WorkspaceJob("opening project " + outputProject.getName()) {
				public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
					outputProject.open(monitor);
					return Status.OK_STATUS;
				}
			});
		}
	}

	private void syncWithWorkspace(final WorkspaceJob job) throws CoreException {
		job.setRule(ResourcesPlugin.getWorkspace().getRoot());
		job.schedule();
	}

	private void removeNatureIfAssigned(IProject project) throws CoreException {
		if (JTransformerPlugin.getNatureIfAvailable(project) != null) {
			JTransformerNature nature = JTransformerPlugin.getNatureIfAvailable(project);
			nature.onClose();
	    	try {
	    		JTDebug.info("JTransformerProjectChangeListener.removeNatureIfAssgined: called clearing persistent factbase");
				JTUtils.clearPersistentFacts(JTUtils.getFactbaseKeyForProject(project));
			} catch (PrologInterfaceException e) {
				JTDebug.report(e);
			}

			JTransformerPlugin.removeNatureFromRegistry(project);
		}

	}


}
