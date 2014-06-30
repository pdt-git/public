package org.cs3.pdt.refactoring;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.editor.internal.editors.PLEditor;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Display;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.ui.handlers.HandlerUtil;


public class PDTRefactoring extends Refactoring{

	public static final String VARIABLE_NEW_TEXT = "NewText";
	public static final String VARIABLE_OFFSET_END = "End";
	public static final String VARIABLE_OFFSET_START = "Start";
	public static final String VARIABLE_FILE = "File";
	
	public static final String REFACTORING_NAME = "PDT Refactoring";

	private PDTRefactoringWizard wizard;
	private PrologProcess prologProcess;
	private PLEditor editor;
	
	private Goal goal;
	
	public void init(ExecutionEvent event){
		
		editor = (PLEditor)HandlerUtil.getActiveEditor(event);
		
		// Get a reference to the running prolog process
		prologProcess = PDTCommonUtil.getActivePrologProcess();
		//TODO this should be done asyncronously and I need a method be be sure it finished
		try {
			prologProcess.start();
		} catch (PrologProcessException e1) {
			e1.printStackTrace();
		}
		
		// Create the Refactoring wizard...
		wizard = new PDTRefactoringWizard(this);

		// Start the wizard!
		RefactoringWizardOpenOperation operation = new RefactoringWizardOpenOperation(wizard);
		
		try {
			operation.run(HandlerUtil.getActiveShell(event), this.getName());
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
	}
	

	
	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {

		return new RefactoringStatus();
	}

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		
		return new RefactoringStatus();
	}
	
	
	
	@Override
	public Change createChange(IProgressMonitor pm) throws CoreException, OperationCanceledException {
		String newText = wizard.getNewText();
		// The prolog predicate that is to be renamed
		
		Display.getDefault().syncExec(new Runnable() {
		    public void run() {
		    	try {
					goal = editor.getSelectedPrologElement();
				} catch (BadLocationException e) {
					e.printStackTrace();
				}
		    }
		});
		
		
		String query = QueryUtils.bT("rename_predicate", "user:"+goal.toString(),"'"+newText+"'",
				VARIABLE_FILE,VARIABLE_OFFSET_START,VARIABLE_OFFSET_END,VARIABLE_NEW_TEXT);
		
		
		
		CompositeChange change = null;
		try {
			List<Map<String, Object>> results = prologProcess.queryAll(query);
			System.out.println(results);
			change = new CompositeChange("Replacements");
			HashMap<String, TextFileChange> changes = new HashMap<>();
			for (Map<String, Object> result : results) {
				try {
					//TODO Check for exceptional results
					//TODO instanceof check
					String pathOut = (String) result.get(VARIABLE_FILE);
					int startOut = Integer.parseInt((String) result.get(VARIABLE_OFFSET_START));
					int endOut = Integer.parseInt((String) result.get(VARIABLE_OFFSET_END));
					String replacementOut = (String) result.get(VARIABLE_NEW_TEXT);
					int offset = startOut;
					int length = endOut-startOut;

					
					//path = Util.unquoteAtom(path); TODO: What do I need this for?
					TextFileChange textFileChange = changes.get(pathOut);
					if (textFileChange == null) {
						//IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(
						//		new Path(pathOut).makeAbsolute());
						IFile file = FileUtils.findFileForLocation(pathOut);
						textFileChange = new TextFileChange(file.getName(), file);
						MultiTextEdit fileChangeRootEdit = new MultiTextEdit();
						textFileChange.setEdit(fileChangeRootEdit);
						changes.put(pathOut, textFileChange);
						change.add(textFileChange);

					}
					
					ReplaceEdit replaceEdit = new ReplaceEdit(offset, length, replacementOut);
					textFileChange.addEdit(replaceEdit);
				} catch (Exception e) {
					Debug.report(e);
				}
			}
		} catch (PrologProcessException e) {
			Debug.report(e);
		}
		return change;
	}

	@Override
	public String getName() {
		return REFACTORING_NAME;
	}


}
