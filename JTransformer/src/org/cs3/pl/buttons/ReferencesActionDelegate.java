package org.cs3.pl.buttons;

import java.io.File;
import java.util.HashMap;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.editors.PLEditor;
import org.cs3.pl.prolog.PrologElementData;
import org.cs3.pl.prolog.PrologManager;
import org.cs3.pl.search.PrologSearchResult;
import org.cs3.pl.search.PrologSearchViewPage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.internal.ui.javaeditor.JavaEditorMessages;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.search.internal.ui.SearchResultViewEntry;
import org.eclipse.search.ui.IGroupByKeyComputer;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResultViewPart;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.internal.editors.text.JavaFileEditorInput;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;


/**
 * @see IWorkbenchWindowActionDelegate
 */
public class ReferencesActionDelegate extends TextEditorAction {
	/**
	 * @author windeln
	 *
	 * To change the template for this generated type comment go to
	 * Window - Preferences - Java - Code Generation - Code and Comments
	 */
	/**
	 * @author windeln
	 *
	 * To change the template for this generated type comment go to
	 * Window - Preferences - Java - Code Generation - Code and Comments
	 */
	public class GroupByKeyComputer implements IGroupByKeyComputer {
		public Object computeGroupByKey(IMarker arg0) {
			return null;
		}
	}
	
	/**
	 *
	 */
	public ReferencesActionDelegate(ITextEditor editor) {
		super(JavaEditorMessages.getResourceBundle(), null, editor);
	}
	
	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
public void run() {
		final PDTPlugin plugin = PDTPlugin.getDefault();
		plugin.getDisplay().asyncExec(new Runnable() {
			public void run() {
				if(PrologManager.getInstance().getClient().isInCall()) {
					String msg = "The predicate search cannot access the Prolog System, because the console may run in debug mode.";
					
					//FIXME
					//fView.getViewSite().getActionBars().getStatusLineManager().setErrorMessage(msg);
					//PDTPlugin.getDefault().setStatusErrorMessage(msg);
					throw new RuntimeException(msg);
				}
				try {
						PrologElementData data = ((PLEditor) plugin
								.getActiveEditor()).getSelectedPrologElement();
						if(data == null)
							return;
						ISearchResultViewPart viewPart = NewSearchUI.activateSearchResultView();
						
						ISearchQuery query = new org.cs3.pl.search.PrologSearchQuery(data);
						
						ProgressMonitorDialog context= new ProgressMonitorDialog(PDTPlugin.getShell());
						IStatus status= NewSearchUI.runQueryInForeground(context, query);
						if (status.isOK()) {
							
							//OccurrencesSearchLabelProvide page = new OccurrencesSearchResultPage();
							//FileSearchPage page = new FileSearchPage();
//							PrologSearchViewPage page = new PrologSearchViewPage();
//							page.setViewPart(NewSearchUI.getSearchResultView());
//							page.init(NewSearchUI.getSearchResultView().getActivePage().getSite());
//							page.createControl((Composite)NewSearchUI.getSearchResultView().getActivePage().getControl());
//							//page.setLayout(AbstractTextSearchViewPage.FLAG_LAYOUT_FLAT);
//							page.setInput(query.getSearchResult(), new HashMap());						
//							System.out.println(status.toString());
						}
				} catch (Exception e) {
					Debug.report(e);
				}
			}
		});
	}
	
	/**
	 * @param result
	 * @param iFile
	 * @return
	 */
	private Integer getLineNumber(String line, IFile iFile) {
		FileEditorInput fei = new FileEditorInput(iFile);
		Document document;
		TextFileDocumentProvider provider = new TextFileDocumentProvider();
		document =(Document)(provider).getDocument(fei);
//		System.out.println("namen: "+getEditorInput().getName());
		int offset = 0;
		try {
			offset = document.getLineInformation(Integer.parseInt(line)-1).getOffset();
		} catch (NumberFormatException e) {
			// TODO Auto-generated catch block
			Debug.report(e);
		} catch (BadLocationException e) {
			// TODO Auto-generated catch block
			Debug.report(e);
		}
		return new Integer(offset);
	}
	
	/**
	 * @return
	 */
		
	public void dispose() {
//		colorManager.dispose();
//		super.dispose();
	}
	
	
	static class FileLabelProvider extends LabelProvider {
		/*
		 * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
		 */
		public String getText(Object element) {
			if(element instanceof SearchResultViewEntry)
				try {
					IMarker marker = ((SearchResultViewEntry)element).getSelectedMarker();
					return getText(((SearchResultViewEntry)element).getResource()) + 
					":" + marker.getAttribute(IMarker.LINE_NUMBER) + ": ("+ marker.getAttribute(IMarker.TEXT) + ")";
				} catch (CoreException e) {
					// TODO Auto-generated catch block
					Debug.report(e);
				}
				if (element instanceof IFile) {
					IPath path=  ((IFile) element).getFullPath();
					return path != null ? path.toString() : ""; //$NON-NLS-1$
				}
				return super.getText(element);
		}
	} 
	
	private String getEditorId(File file) {
		return "org.cs3.pl.editors.PLEditor"; //$NON-NLS-1$
	}
	
	private IEditorInput createEditorInput(File file) {
		IFile workspaceFile= getWorkspaceFile(file);
		if (workspaceFile != null)
			return new FileEditorInput(workspaceFile);
		return new JavaFileEditorInput(file);
	}
	
	private IFile getWorkspaceFile(File file) {
		IWorkspace workspace= ResourcesPlugin.getWorkspace();
		IPath location= new Path(file.getAbsolutePath());
		IFile[] files= workspace.getRoot().findFilesForLocation(location);
		if (files == null || files.length == 0)
			return null;
		if (files.length == 1)
			return files[0];
		return selectWorkspaceFile(files);
	}
	
	private IFile selectWorkspaceFile(IFile[] files) {
		ElementListSelectionDialog dialog= new ElementListSelectionDialog(PDTPlugin.getDefault().getActiveWorkbenchWindow().getShell(), new FileLabelProvider());
		dialog.setElements(files);
		dialog.setTitle("Select Workspace File");
		dialog.setMessage("The selected file is referenced by multiple linked resources in the workspace.\nPlease select the workspace resource you want to use to open the file.");
		if (dialog.open() == Window.OK)
			return (IFile) dialog.getFirstResult();
		return null;
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		
	} 
	
	
//	class ReferenceSearch extends WorkspaceModifyOperation{
//		
//		
//		PrologElementData data;
//		ReferenceSearch(PrologElementData data){
//			this.data  = data;
//			
//		}
//		/* (non-Javadoc)
//		 * @see org.eclipse.ui.actions.WorkspaceModifyOperation#execute(org.eclipse.core.runtime.IProgressMonitor)
//		 */
//		protected void execute(IProgressMonitor arg0) throws CoreException, InvocationTargetException, InterruptedException {
////			NewSearchUI.activateSearchResultView();
////			PrologElementData data = ((PLEditor) PDTPlugin.getDefault()
////					.getActiveEditor()).getSelectedPrologElement();			
////			ISearchQuery query = new PrologSearchQuery(data);
////			ProgressMonitorDialog context= new ProgressMonitorDialog(arg0.getSite().getShell());
////			IStatus status= NewSearchUI.runQueryInForeground(context, query);
//			
//	}
//
//	/* (non-Javadoc)
//	 * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction, org.eclipse.ui.IEditorPart)
//	 */
//	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
//		// TODO Auto-generated method stub
//		
//	}
	
}
