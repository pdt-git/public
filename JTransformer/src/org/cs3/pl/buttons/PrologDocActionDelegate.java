package org.cs3.pl.buttons;

import java.io.File;
import java.io.IOException;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.doc.PrologDocWriter;
import org.cs3.pl.parser.PrologCompiler;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IPathEditorInput;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.FileEditorInput;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class PrologDocActionDelegate implements IWorkbenchWindowActionDelegate {
	private IWorkbenchWindow window;

	/**
	 *
	 */
	public PrologDocActionDelegate() {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run(IAction action)  {
		
		//The Thread is needed, because the consult processing may be part of  
		// a tracing/debugging session. The output the console must 
		// wait for the calling Runnable to finish -> deadlock in the Eclipse Display Scheduler
		Thread consulter =  new Thread(){
			public void run() {
				final IEditorPart editorPart = PDTPlugin.getDefault().getActiveEditor();
				boolean dirty = editorPart.isDirty();
				if (dirty)
					editorPart.doSave(null);

				IFile file = ((FileEditorInput)editorPart.getEditorInput()).getFile();
				final String filename = file.getFullPath().toString();

				try {
					PrologCompiler checker = new PrologCompiler();
					checker.compile(file);
					PrologDocWriter docwriter = new PrologDocWriter(PDTPlugin.getDefault().getWorkspaceLocation());
					docwriter.write(checker.getModule());
					String htmlFile = docwriter.getDocumentationFile(checker.getModule());
					
					openFile(htmlFile);
					
				} catch (CoreException e) {
					Debug.report(e);
				} catch (IOException e) {
					Debug.report(e);
				}

					
			}
		};
		consulter.start();
		
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
		this.window = window;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction, org.eclipse.ui.IEditorPart)
	 */
	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		// TODO Auto-generated method stub
		
	}
	
	public void openFile(final String filename) {
		File file= new File(filename);
		if (file != null && file.exists()) {
			final IEditorInput input= new ExternalPathEditorInput(filename); 
			final String editorId= IEditorRegistry.SYSTEM_EXTERNAL_EDITOR_ID; //getEditorId(file);
			final IWorkbenchPage page= PDTPlugin.getDefault().getActivePage();
				PDTPlugin.getDefault().getDisplay().syncExec(new Runnable() {
					public void run() {
						try {
							page.openEditor(input, editorId,true);
						} catch (PartInitException e) {
							e.printStackTrace();
						}

					}
				});
		} else if (file != null) {
			Debug.error("could not open file: "+ filename);
		}
	}

	class ExternalPathEditorInput implements IPathEditorInput {
		
		private String filename;

		public ExternalPathEditorInput(String filename) {
			this.filename = filename;
		}
		public IPath getPath() {
			return new Path(filename);
		}
		public boolean exists() {
			return true;
		}
		public ImageDescriptor getImageDescriptor() {
			return null;
		}
		public String getName() {
			return filename;
		}
		public IPersistableElement getPersistable() {
			return null;
		}
		public String getToolTipText() {
			return null;
		}
		public Object getAdapter(Class adapter) {
			return null;
		}
		
	};
}
