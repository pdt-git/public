package org.cs3.pdt.internal.views;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.TreeViewer;

public class ToggleSortAction extends Action {
	private TreeViewer treeViewer;

	public ToggleSortAction(TreeViewer treeViewer) {
		super("Sort", AS_CHECK_BOX);
		this.setImageDescriptor(ImageRepository
				.getImageDescriptor(ImageRepository.SORT));
		this.treeViewer = treeViewer;
		String val = PDTPlugin.getDefault().getPreferenceValue(PDT.PREF_OUTLINE_SORT, "false");
		if("true".equalsIgnoreCase(val)){
			setChecked(true);
			treeViewer.setSorter(new LexicalPrologOutlineSorter());	
		}
		else{
			setChecked(false);
			treeViewer.setSorter(new PositionalPrologOutlineSorter());
		}
	}

	@Override
	public void run() {
		if(isChecked()){
			treeViewer.setSorter(new LexicalPrologOutlineSorter());
			PDTPlugin.getDefault().setPreferenceValue(PDT.PREF_OUTLINE_SORT, "true");
		}
		else{
			treeViewer.setSorter(new PositionalPrologOutlineSorter());
			PDTPlugin.getDefault().setPreferenceValue(PDT.PREF_OUTLINE_SORT, "false");
		}
	}
}
