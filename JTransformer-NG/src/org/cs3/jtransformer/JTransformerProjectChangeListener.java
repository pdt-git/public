package org.cs3.jtransformer;

import org.cs3.jtransformer.util.JTUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

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
					IProject project = (IProject) resource;
					try {
						if (delta.getFlags() == IResourceDelta.OPEN) {
							if (!project.isOpen()) {
								removeNatureIfAssigned(project);
								closeOutputProject(project);
							} else {
								if (project.hasNature(JTransformer.NATURE_ID)) {
									openOutputProject(project);
									JTransformerPlugin.getNature(project);
								}
							}
						} else if (delta.getKind() == IResourceDelta.REMOVED) {
							removeNatureIfAssigned(project);
							deleteOutputProject(project);

						}
					} catch (CoreException e) {
						 JTUtils.logAndDisplayUnknownError(e);
					}
				}

			}
		} 
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
			JTransformerPlugin.getNatureIfAvailable(project).onClose();
			JTransformerPlugin.removeNatureFromRegistry(project);
		}
	}

}
