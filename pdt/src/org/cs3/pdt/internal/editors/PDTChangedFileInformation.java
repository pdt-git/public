package org.cs3.pdt.internal.editors;

import java.io.File;

import org.cs3.pl.common.Util;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorInput;
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
		return editorInput.getName();
	}

	@Override
	public boolean isEmpty() {
		return false;
	}
	
}
