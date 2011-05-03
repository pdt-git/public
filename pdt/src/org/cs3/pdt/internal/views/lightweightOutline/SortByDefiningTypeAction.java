package org.cs3.pdt.internal.views.lightweightOutline;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.custom.BusyIndicator;

class SortByDefiningTypeAction extends Action {

		/**
		 * 
		 */
		private final PrologOutlineInformationControl prologOutlineInformationControl;

		private static final String STORE_SORT_BY_DEFINING_TYPE_CHECKED= "SortByDefiningType.isChecked"; //$NON-NLS-1$

		private TreeViewer fOutlineViewer;

		/**
		 * Creates the action.
		 *
		 * @param outlineViewer the outline viewer
		 * @param prologOutlineInformationControl TODO
		 */
		private SortByDefiningTypeAction(PrologOutlineInformationControl prologOutlineInformationControl, TreeViewer outlineViewer) {
			//TRHO:TODO
			super("sort");//TextMessages.JavaOutlineInformationControl_SortByDefiningTypeAction_label);
//			setDescription(TextMessages.JavaOutlineInformationControl_SortByDefiningTypeAction_description);
//			setToolTipText(TextMessages.JavaOutlineInformationControl_SortByDefiningTypeAction_tooltip);
//
//			JavaPluginImages.setLocalImageDescriptors(this, "definingtype_sort_co.gif"); //$NON-NLS-1$
			this.prologOutlineInformationControl = prologOutlineInformationControl;

			fOutlineViewer= outlineViewer;

//			PlatformUI.getWorkbench().getHelpSystem().setHelp(this, IJavaHelpContextIds.SORT_BY_DEFINING_TYPE_ACTION);

			//boolean state= getDialogSettings().getBoolean(STORE_SORT_BY_DEFINING_TYPE_CHECKED);
			setChecked(false);
		}

		/*
		 * @see Action#actionPerformed
		 */
		public void run() {
			BusyIndicator.showWhile(fOutlineViewer.getControl().getDisplay(), new Runnable() {
				public void run() {
//					fInnerLabelProvider.setShowDefiningType(isChecked());
					SortByDefiningTypeAction.this.prologOutlineInformationControl.getDialogSettings().put(STORE_SORT_BY_DEFINING_TYPE_CHECKED, isChecked());

					SortByDefiningTypeAction.this.prologOutlineInformationControl.setMatcherString(SortByDefiningTypeAction.this.prologOutlineInformationControl.fPattern, false);
					fOutlineViewer.refresh(true);

					// reveal selection
					Object selectedElement= SortByDefiningTypeAction.this.prologOutlineInformationControl.getSelectedElement();
					if (selectedElement != null)
						fOutlineViewer.reveal(selectedElement);
				}
			});
		}
	}