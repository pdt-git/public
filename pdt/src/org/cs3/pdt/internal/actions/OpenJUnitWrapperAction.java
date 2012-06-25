package org.cs3.pdt.internal.actions;

import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.ui.util.UIUtils;
import org.eclipse.jdt.junit.model.ITestCaseElement;
import org.eclipse.jdt.junit.model.ITestSuiteElement;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

public class OpenJUnitWrapperAction implements IObjectActionDelegate {

	private String fileName;
	private int lineNumber;



	@Override
	public void run(IAction action) {
		IEditorPart editorPart = UIUtils.openInEditor(fileName);
		if (editorPart != null && editorPart instanceof PLEditor){
			((PLEditor) editorPart).gotoLine(lineNumber);
		}
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		
		ITreeSelection tree = (ITreeSelection)selection;
		ITestCaseElement tc = (ITestCaseElement)tree.getFirstElement();
		fileName = ((ITestSuiteElement)tc.getParentContainer().getParentContainer()).getSuiteTypeName();
		String[] args= tc.getTestMethodName().substring(6,tc.getTestMethodName().length()-1).split(":");
//		String[] fileNum= args[1].split(";");
//		fileName = fileNum[0];
		if(tc.getFailureTrace()!=null && tc.getFailureTrace().getTrace().startsWith("java.lang.AssertionError: Failed assertion in line ")){
			String trace = tc.getFailureTrace().getTrace();
			trace = trace.substring("java.lang.AssertionError: Failed Assertion in Line ".length(),trace.length());
			StringBuffer buf = new StringBuffer();
			int i = 0;
			while(trace.charAt(i) >='0' && trace.charAt(i) <='9' ){
				buf.append(trace.charAt(i));
				i++;
			}
			lineNumber = Integer.parseInt(buf.toString());
						
		} else

		lineNumber = Integer.parseInt(args[1]);
		
	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		// TODO Auto-generated method stub

	}

}
