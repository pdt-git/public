/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.core;

import org.cs3.pl.common.Util;
import org.eclipse.core.resources.IFile;
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

	public static int convertCharacterOffset(String data, int offset) {
		String value = PDTCorePlugin.getDefault().getPreferenceValue(
				PDTCore.PREF_CONVERT_CHARACTER_OFFSETS, "true");
		if ("true".equalsIgnoreCase(value)) {
			return Util.logicalToPhysicalOffset(data, offset);
		}
		return offset;
	}

	
	
	public static IPrologProject getPrologProject(IResource file) throws CoreException {
		if (file == null) {
			return null;
		}
		IProject project = file.getProject();

		if (project != null && project.hasNature(PDTCore.NATURE_ID)) {
			return (IPrologProject) project.getNature(PDTCore.NATURE_ID);
		}
		else{
			return null;
		}
	}
}
