/* $LICENSE_MSG$ */

package org.cs3.pdt.internal.search;

import org.cs3.pdt.internal.structureElements.SearchFileTreeElement;
import org.cs3.prolog.common.ExternalPrologFilesProjectUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.DecoratingStyledCellLabelProvider;
import org.eclipse.ui.PlatformUI;

public class DecoratingPrologSearchLabelProvider extends DecoratingStyledCellLabelProvider {
	
	public DecoratingPrologSearchLabelProvider(IStyledLabelProvider labelProvider) {
		super(labelProvider, PlatformUI.getWorkbench().getDecoratorManager().getLabelDecorator(), null);
	}
	
	@Override
	public String getToolTipText(Object element) {
		if (element instanceof SearchFileTreeElement) {
			SearchFileTreeElement fileTreeElement = (SearchFileTreeElement) element;
			IFile file = fileTreeElement.getFile();
			if (ExternalPrologFilesProjectUtils.isExternalFile(file)) {
				return file.getRawLocation().toOSString();
			} else {
				return file.getFullPath().toOSString();
			}
		} else {
			return null;
		}
	}
}

