/*
 * Created on 31.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.views;

import java.io.IOException;
import java.util.ArrayList;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.PrologElement;
import org.cs3.pl.editors.PLEditor;
import org.cs3.pl.fileops.MetaDataManagerFactory;
import org.cs3.pl.fileops.PrologMetaDataManager;
import org.cs3.pl.parser.PrologCompiler;
import org.cs3.pl.prolog.IMetaInfoProvider;
import org.cs3.pl.prolog.PrologElementData;
import org.cs3.pl.prolog.PrologHelper;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.AbstractTextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PrologOutline extends ContentOutlinePage {
	private IEditorInput input;
	private AbstractTextEditor editor;
	private IDocumentProvider documentProvider;
//	private TreeViewer viewer;
	private ContentProvider contentProvider;
	
	public static class ContentProvider implements ITreeContentProvider {

		private PrologElement[] predicates = new PrologElement[0];
		private IDocumentProvider documentProvider;
		/**
		 * @param filtering
		 */
		public ContentProvider(IDocumentProvider documentProvider) {
			this.documentProvider = documentProvider;
		}

		/**
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		public void dispose() {
		}
		
		/**
		 * 
		 * 
		 * do nothing
		 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(Viewer, Object, Object)
		 */
		public void inputChanged(final Viewer viewer, Object oldInput, Object newInput) {
			// TODO: Do not do this on the gui thread!!! (e.g. PLEditor.updateOutline())
			try {
				String filename = PDTPlugin.getDefault().getActiveFileName();
				//ld: i guess it is safe to simply return, if filename is null.
				if(filename==null)return;
				
				//PDTPlugin.getDefault().activatePrologConsole();
				String rawFilename = PDTPlugin.getDefault()
						.getActiveRawFileName();
				PrologMetaDataManager mdManager = MetaDataManagerFactory.getPrologMetaDataManager(PrologManager.getInstance().getHiddenClient(),PrologMetaDataManager.MODEL);
				if(!mdManager.exists(filename)) {
					PrologCompiler compiler = new PrologCompiler();
					compiler.setAddMarkers(false);
					compiler.compile(PDTPlugin.getDefault().getActiveFile());
					mdManager.saveMetaDataForClauses(filename, compiler);
				}

				mdManager.consult(filename);
				IMetaInfoProvider metaInfo = new PrologHelper(PrologManager.getInstance().getHiddenClient());
				PrologElementData[] data = metaInfo.retrievePrologElements(filename);//getCachedPrologElements(filename);
				if (data == null || data.length == 0) {
					predicates = new PrologElement[0];
					return;
				}
				ArrayList l = new ArrayList();
				l.add(data[0]);
				for (int i = 1; i < data.length; i++)
					if (!data[i].belongsToSamePredicate(data[i - 1]))
						l.add(data[i]);
				ArrayList l2 = new ArrayList();
/*				for (int i = l.size() - 1; i >= 0; i--)
					l2.add(l.get(i));
*/
				for (int i = 0; i < l.size(); i++)
					l2.add(l.get(i));
				predicates = PrologElement.fromData((PrologElementData[]) l2
						.toArray(new PrologElementData[0]));
			} catch (Exception e) {
				Debug.report(e);
			}
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
		 */
		public Object[] getChildren(Object parentElement) {
			return predicates;
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
		 */
		public Object getParent(Object element) {
			return null;
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
		 */
		public boolean hasChildren(Object element) {
			return false;
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		public Object[] getElements(Object inputElement) {
			return predicates;
		}
			

}
	/**
	 * @param provider
	 * @param editor2
	 */
	public PrologOutline(IDocumentProvider provider, AbstractTextEditor editor) {
		
		this.editor =editor;
		this.documentProvider =provider;
	}


	public void createControl(Composite parent) {
		super.createControl(parent);
//		SashForm composite = new SashForm(parent,SWT.VERTICAL);//new Composite(parent, SWT.NONE);
		
//		parent.setLayout(new FillLayout());
		
		TreeViewer viewer = getTreeViewer();
		
		contentProvider =new ContentProvider(documentProvider);
		viewer.setContentProvider(contentProvider);
		viewer.setLabelProvider(new WorkbenchLabelProvider());
//		viewer.getLabelProvider().
		viewer.addSelectionChangedListener(this);	
		if (input != null)
			viewer.setInput(input);
		
		//int[] weights = {100};
		//composite.setWeights(weights);
		
	
	}


//	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
//		System.out.println("selection changed");
//		if (newInput != null) {
//			IDocument document= documentProvider.getDocument(newInput);
//			if (document != null && viewer instanceof TreeViewer) {
//				((TreeViewer)viewer).add(null, "TestOUT");
//			}
//		}
//	}	
	
		public TreeViewer getTreeViewer() {
			return super.getTreeViewer();
		}

//		public void createControl(Composite parent) {
//
//		//viewer= new TreeViewer(parent);
//		super.createControl(parent);
//		}
	

	public void setInput(IEditorInput input) {
		this.input = input;
	}


	public IEditorInput getInput() {
		return input;
	}


	/**
	 * @param input2
	 * @param b
	 * @return
	 */
//	private IAdaptable getContentOutline(IFile input2, boolean b) {
//		FileStructureView structureView = new FileStructureView();
//	}

	public void selectionChanged(SelectionChangedEvent event) {
		if (!((StructuredSelection)event.getSelection()).isEmpty()) {
			PrologElement element = ((PrologElement)((StructuredSelection)event.getSelection()).getFirstElement());
			int pos = element.getPosition();
			int len = element.getLength();
			((PLEditor)PDTPlugin.getDefault().getActiveEditor()).selectAndReveal(pos, len);
		}
	}


	public ContentProvider getContentProvider() {
		return contentProvider;
	}	
}
