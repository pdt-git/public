package org.cs3.pl.buttons;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.astvisitor.CompilationUnitProvider;
import org.cs3.pl.astvisitor.SectionFactGenerator;
import org.cs3.pl.astvisitor.VariableIdResolver;
import org.cs3.pl.astvisitor.VariableTypeResolver;
import org.cs3.pl.fileops.MockPrologWriter;
import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
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
		try {
		PDTPlugin plugin = PDTPlugin.getDefault();
		TextSelection selection = plugin.getSelection();
		System.out.println(selection.getOffset() + ", " + selection.getLength());
		CompilationUnitProvider cuProvider = new CompilationUnitProvider();
		IFile file = ((FileEditorInput)plugin.getActiveEditor().getEditorInput()).getFile();
		ICompilationUnit icu = cuProvider.createICompilationUnit(file);
		
		MockPrologWriter writer = new MockPrologWriter(',');
		CompilationUnit root = PDTPlugin.getDefault().parseICompilationUnit(icu);
		VariableTypeResolver typeResolver = new VariableTypeResolver();
		SectionFactGenerator visitor = new SectionFactGenerator( icu,file.getFullPath().toString(),
				new VariableIdResolver(), typeResolver, writer,selection);
		root.accept(visitor);
		writer.close();
		if (writer.getLast().length() > 0) {
			plugin.getPrologConsole().getQueryWidget().setText(
					typeResolver.getTypeBindungs()+
					writer.getLast().substring(0,writer.getLast().length()-1)
					);
		}
		}catch (Exception e){
			Debug.report(e);
		}
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#selectionChanged
	 */
	public void selectionChanged(IAction action, ISelection selection)  {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#dispose
	 */
	public void dispose()  {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#init
	 */
	public void init(IWorkbenchWindow window)  {
	}
	
	
}
