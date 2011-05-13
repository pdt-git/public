package org.cs3.pdt.internal.views.lightweightOutline;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.custom.BusyIndicator;

class LexicalSortingAction extends Action {

		/**
		 * 
		 */
//		private final PrologOutlineInformationControl prologOutlineInformationControl;

//		private static final String STORE_LEXICAL_SORTING_CHECKED= "LexicalSortingAction.isChecked"; //$NON-NLS-1$

		private TreeViewer fOutlineViewer;

		LexicalSortingAction(/*PrologOutlineInformationControl prologOutlineInformationControl,*/ TreeViewer outlineViewer) {
			super("lexicalsorting"/*TextMessages.JavaOutlineInformationControl_LexicalSortingAction_label*/, IAction.AS_CHECK_BOX);
//			this.prologOutlineInformationControl = prologOutlineInformationControl;
			setToolTipText("lexicalsorting");//TextMessages.JavaOutlineInformationControl_LexicalSortingAction_tooltip);
			setDescription("lexicalsorting");//TextMessages.JavaOutlineInformationControl_LexicalSortingAction_description);


			fOutlineViewer= outlineViewer;

//			boolean checked=getDialogSettings().getBoolean(STORE_LEXICAL_SORTING_CHECKED);
			//TODO
			setChecked(true);
//			PlatformUI.getWorkbench().getHelpSystem().setHelp(this, IJavaHelpContextIds.LEXICAL_SORTING_BROWSING_ACTION);
		}

		public void run() {
			valueChanged(isChecked(), true);
		}

		private void valueChanged(final boolean on, boolean store) {
			setChecked(on);
			BusyIndicator.showWhile(fOutlineViewer.getControl().getDisplay(), new Runnable() {
				public void run() {
					fOutlineViewer.refresh(false);
				}
			});

//			if (store)
//				this.prologOutlineInformationControl.getDialogSettings().put(STORE_LEXICAL_SORTING_CHECKED, on);
		}
	}