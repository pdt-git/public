package org.cs3.pdt.core;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

public final class PDTCoreUtils {
	/**
	 * @param ipd
	 * @return
	 * @throws CoreException
	 */
	public static void addPDTNature(IProject project) throws CoreException {

		IProjectDescription ipd = project.getDescription();
		String[] oldNIDs = ipd.getNatureIds();
		String[] newNIDs = new String[oldNIDs.length + 1];
		newNIDs[0] = PDTCore.NATURE_ID;
		System.arraycopy(oldNIDs, 0, newNIDs, 1, oldNIDs.length);
		ipd.setNatureIds(newNIDs);
		if (!project.isSynchronized(IResource.DEPTH_ONE)) {
			project.refreshLocal(IResource.DEPTH_ONE, null);
		}
		project.setDescription(ipd, null);

	}

	/**
	 * @param project
	 * @return
	 * @throws CoreException
	 */
	public static void removePDTNature(IProject project) throws CoreException {
		if (project.hasNature(PDTCore.NATURE_ID)) {
			IProjectDescription ipd = project.getDescription();
			String[] oldNIDs = ipd.getNatureIds();
			String[] newNIDs;
			newNIDs = new String[oldNIDs.length - 1];
			int j = 0;
			for (int i = 0; i < newNIDs.length; i++) {
				if (oldNIDs[j].equals(PDTCore.NATURE_ID))
					j++;
				newNIDs[i] = oldNIDs[j];
				j++;
			}
			ipd.setNatureIds(newNIDs);
			if (!project.isSynchronized(IResource.DEPTH_ONE)) {
				project.refreshLocal(IResource.DEPTH_ONE, null);
			}
			project.setDescription(ipd, null);
		}
	}
}
