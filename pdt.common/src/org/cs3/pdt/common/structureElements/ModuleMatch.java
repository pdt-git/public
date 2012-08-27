package org.cs3.pdt.common.structureElements;

import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

public class ModuleMatch extends Match {

	private String module;
	private IFile file;

	public ModuleMatch(SearchModuleElement element, String module, IFile file, int line) {
		super(element, UNIT_LINE, line, 1);
		this.module = module;
		this.file = file;
	}

	public String getModule() {
		return module;
	}

	public IFile getFile() {
		return file;
	}

}
