package org.cs3.pdt.internal.views;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.jface.action.Action;

public class ToggleSortAction extends Action {
	private PrologOutline outline;

	ToggleSortAction(PrologOutline outline) {
		super("Sort", AS_CHECK_BOX);
		this.setImageDescriptor(ImageRepository
				.getImageDescriptor(ImageRepository.SORT));
		this.outline = outline;
		String val = PDTPlugin.getDefault().getPreferenceValue(PDT.PREF_OUTLINE_SORT, "false");
		if(Boolean.parseBoolean(val)){
			setChecked(true);
			outline.getTreeViewer().setSorter(new LexicalPrologOutlineSorter());	
		}
		else{
			setChecked(false);
			outline.getTreeViewer().setSorter(new PositionalPrologOutlineSorter());
		}
		
	}

	public void run() {
		if(isChecked()){
			outline.getTreeViewer().setSorter(new LexicalPrologOutlineSorter());
			PDTPlugin.getDefault().setPreferenceValue(PDT.PREF_OUTLINE_SORT, "true");
		}
		else{
			outline.getTreeViewer().setSorter(new PositionalPrologOutlineSorter());
			PDTPlugin.getDefault().setPreferenceValue(PDT.PREF_OUTLINE_SORT, "false");
		}
	}
	
	
}
