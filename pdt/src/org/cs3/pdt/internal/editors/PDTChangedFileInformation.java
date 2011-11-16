package org.cs3.pdt.internal.editors;

import java.io.File;

import org.cs3.pl.common.Util;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.internal.filesystem.local.LocalFile;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.part.FileEditorInput;

public class PDTChangedFileInformation implements ISelection{
	private IEditorInput editorInput;
	
	public PDTChangedFileInformation(IEditorInput input) {
		editorInput = input;
	}
	
	public String getPrologFileName() {
		if( editorInput instanceof FileEditorInput) {
			FileEditorInput fileEditorInput = (FileEditorInput)editorInput;
			IPath path = fileEditorInput.getPath();
			File file = path.toFile();
			return Util.prologFileName(file);
		}
		if( editorInput instanceof FileStoreEditorInput) {
			FileStoreEditorInput e = (FileStoreEditorInput)editorInput;
			File file = new File(e.getURI());
			return Util.prologFileName(file);
		}
		return editorInput.getName();
	}

	@Override
	public boolean isEmpty() {
		return false;
	}
	
}
