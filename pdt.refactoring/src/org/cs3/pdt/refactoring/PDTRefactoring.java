package org.cs3.pdt.refactoring;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.editor.internal.editors.PLEditor;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.handlers.HandlerUtil;


public class PDTRefactoring extends Refactoring{

	public static final String VARIABLE_REPLACEMENT = "Replacement";
	public static final String VARIABLE_OFFSET_END = "OffsetEnd";
	public static final String VARIABLE_OFFSET_START = "OffsetStart";
	public static final String VARIABLE_FILE = "File";
	
	public static final String REFACTORING_NAME = "PDT Refactoring";

	private IFile file;
	private TextSelection selection;
	private PDTRefactoringWizard wizard;
	private PrologProcess prologProcess;
	
	public void init(ExecutionEvent event){
		
		// Is the active editor a prolog editor?
		TextEditor editor = (TextEditor)HandlerUtil.getActiveEditor(event);
		if(! (editor instanceof PLEditor)){
			System.out.println("The active editor is not a prolog editor!");
		}
		else{
			editor = (PLEditor)editor;
		}
		
		// Get the active file TODO: Not a save cast
		IFileEditorInput editorInput = (IFileEditorInput) (HandlerUtil.getActiveEditorInput(event));
		file = editorInput.getFile();
		
		
		// Get the current selection TODO: Not a save cast
		selection = (TextSelection) HandlerUtil.getCurrentSelection(event);
		
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
		int offset = this.selection.getOffset();
		int length = this.selection.getLength();
		String newText = wizard.getNewText();
		String path = file.getFullPath().toOSString();
		
		String query = QueryUtils.bT("rename", "['"+path+"', "+
				+ offset +","+length+"],'"+newText+"', TextChange");
		
		
		
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
					Vector<String> textChangeList = (Vector<String>) result.get("TextChange");
								
										
					String pathOut = textChangeList.get(0);
					int offsetOut = Integer.parseInt(textChangeList.get(1));
					int lengthOut = Integer.parseInt(textChangeList.get(2));
					String replacementOut =  textChangeList.get(3);
					
					//path = Util.unquoteAtom(path); TODO: What do I need this for?
					TextFileChange textFileChange = changes.get(pathOut);
					if (textFileChange == null) {
						IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(
								new Path(pathOut).makeAbsolute());
						textFileChange = new TextFileChange(file.getName(), file);
						MultiTextEdit fileChangeRootEdit = new MultiTextEdit();
						textFileChange.setEdit(fileChangeRootEdit);
						changes.put(pathOut, textFileChange);
					}
					
					ReplaceEdit replaceEdit = new ReplaceEdit(offsetOut, lengthOut, replacementOut);
					textFileChange.addEdit(replaceEdit);
					change.add(textFileChange);
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
