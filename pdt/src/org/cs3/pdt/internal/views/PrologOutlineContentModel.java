package org.cs3.pdt.internal.views;

import java.io.File;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCoreUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IFileEditorInput;

final class PrologOutlineContentModel extends ContentModel {
	/**
	 * 
	 */
	private final PrologOutline prologOutline;

	/**
	 * @param prologOutline
	 */
	PrologOutlineContentModel(PrologOutline prologOutline) {
		this.prologOutline = prologOutline;
	}

	@Override
	public File getFile() {
		try {
			IFileEditorInput editorInput = null;
			IFile file = null;
			IPrologProject plProject = null;

			if (this.prologOutline.input instanceof IFileEditorInput) {
				editorInput = (IFileEditorInput) this.prologOutline.input;
			}
			if (editorInput != null) {
				file = editorInput.getFile();
				plProject = PDTCoreUtils.getPrologProject(file);
			}

			if (plProject == null) {
				file = null;
			}
			return file == null ? null : file.getLocation().toFile();
		} catch (Exception e) {
			return null;
		}
	}
}