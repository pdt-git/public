package org.cs3.jtransformer.internal.actions;

import java.util.HashSet;
import java.util.Set;

import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.internal.astvisitor.SectionFactGenerator;
import org.cs3.jtransformer.internal.astvisitor.VariableIdResolver;
import org.cs3.jtransformer.internal.astvisitor.VariableTypeResolver;
import org.cs3.jtransformer.internal.bytecode.ITypeFQNManager;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FactsToClipboard implements IWorkbenchWindowActionDelegate {
	/**
	 *
	 */
	public FactsToClipboard() {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
public void run(IAction action)  {
		final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		try {
			IEditorPart editor = UIUtils.getActiveEditor();
			if( editor == null)
				showError("No Editor open in workspace");
			ITextSelection selection = (ITextSelection)editor.getEditorSite().getSelectionProvider().getSelection(); 
				
			// plugin.getSelection();
			System.out.println(selection.getOffset() + ", " + selection.getLength());
			IFile file = ((FileEditorInput)editor.getEditorInput()).getFile();
			ICompilationUnit icu = JavaCore.createCompilationUnitFrom(file);
			
			Set filter = new HashSet();
			filter.add("slT");
			FilteredStringBufferWriter writer = new FilteredStringBufferWriter(
					filter,',') {
				
			};
			CompilationUnit root = parseICompilationUnit(icu);
			VariableIdResolver idResolver = new VariableIdResolver();
			VariableTypeResolver typeResolver = new VariableTypeResolver(new ITypeFQNManager(idResolver),idResolver);
			SectionFactGenerator visitor = new SectionFactGenerator( icu,file.getFullPath().toString(),
					idResolver, typeResolver, writer,selection);
			root.accept(visitor);
			writer.close();
			if (writer.getLast().length() > 0) {
				Clipboard cb = new Clipboard(shell.getDisplay());
				TextTransfer textTransfer = TextTransfer.getInstance();
				String textData = typeResolver.getTypeBindungs()+
								writer.getLast().substring(0,writer.getLast().length()-1);
	
				cb.setContents(new Object[]{textData}, new Transfer[]{textTransfer});
				MessageDialog.openInformation(shell,"Copied corresponding facts to clipboard", textData);
			}
		}catch (Exception e){
			MessageDialog.openError(shell,"Error occurred while generating facts", e.getLocalizedMessage());
			e.printStackTrace();
			Debug.report(e);
		}
	}
	ASTParser parser = ASTParser.newParser(AST.JLS3);

	public CompilationUnit parseICompilationUnit(ICompilationUnit icu) {
		parser.setSource(icu);
		parser.setResolveBindings(true);
		CompilationUnit root=(CompilationUnit)parser.createAST(null);
		return root;
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#selectionChanged
	 */
	public void selectionChanged(IAction action, ISelection selection) {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#dispose
	 */
	public void dispose() {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#init
	 */
	public void init(IWorkbenchWindow window) {
	}

	private void showError(final String msg) {
		final Shell shell = JTransformerPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
		shell.getDisplay().asyncExec(new Runnable() {
			public void run(){
				MessageDialog.openError(shell,"Compile all operation", msg);
			}
		});
	}
}
