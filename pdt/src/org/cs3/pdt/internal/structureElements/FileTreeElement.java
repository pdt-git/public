package org.cs3.pdt.internal.structureElements;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;

public class FileTreeElement implements PDTTreeElement{
	private List<PDTTreeElement> elements= new ArrayList<PDTTreeElement>();
	private IFile file;
	
	public FileTreeElement(IFile file) {
		this.file = file;
	}
	
	public IFile getFile() {
		return file;
	}
	
	public void addChild(PDTTreeElement elem) {
		elements.add(elem);
	}

	@Override
	public boolean hasChildren() {
		return !(elements.isEmpty());
	}

	@Override
	public Object[] getChildren() {
		return elements.toArray();
	}

	@Override
	public String getLabel() {
		return file.getFullPath().toString();
	}

}
