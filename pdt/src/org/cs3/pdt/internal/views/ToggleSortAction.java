package org.cs3.pdt.internal.views;

import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.jface.action.Action;

public class ToggleSortAction extends Action {
	private PrologOutline outline;

	ToggleSortAction(PrologOutline outline) {
		super("Sort", AS_CHECK_BOX);
		this.setImageDescriptor(ImageRepository
				.getImageDescriptor(ImageRepository.SORT));
		this.outline = outline;
	}

	@Override
	public void run() {
		if(isChecked()){
			outline.getTreeViewer().setSorter(new LexicalPrologOutlineSorter());	
		}
		else{
			outline.getTreeViewer().setSorter(new PositionalPrologOutlineSorter());
		}
	}
	
	
}
