/*
 * Created on 23.08.2004
 *
 */
package org.cs3.pdt.internal.search;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.search.ui.text.AbstractTextSearchViewPage;
import org.eclipse.search.ui.text.Match;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;


public class PrologSearchViewPage extends AbstractTextSearchViewPage {

	private TextSearchTableContentProvider fContentProvider;
	protected static final Image IMAGE = ImageRepository.getImage(ImageRepository.PE_PUBLIC).createImage();

	public PrologSearchViewPage(){
		super(AbstractTextSearchViewPage.FLAG_LAYOUT_FLAT);
		init(NewSearchUI.getSearchResultView().getActivePage().getSite());
	}
	
	protected void elementsChanged(Object[] objects) {
		if (fContentProvider != null)
			fContentProvider.elementsChanged(objects);
		System.out.println("changed");
	}

	protected void clear() {
		if (fContentProvider != null)
			fContentProvider.clear();
	}

	protected void configureTreeViewer(TreeViewer viewer) {
		// TODO Auto-generated method stub
		System.out.println("confTreeV");
		throw new IllegalStateException("Doesn't support tree mode."); //$NON-NLS-1$
	}

	protected void configureTableViewer(TableViewer viewer) {
		//viewer.setSorter(new JavaElementLineSorter());
		viewer.setLabelProvider(new ILabelProvider(){

			public Image getImage(Object element) {
				//TODO: correct image
				return IMAGE;
			}

			public String getText(Object element) {
				int count = PrologSearchViewPage.this.getDisplayedMatchCount(element);
				String plural = (count==1)?"":"es";
				return ((IFile)element).getFullPath().toString()+ " (" + count +" match"+plural+")";
			}

			public void addListener(ILabelProviderListener listener) {
			}

			public void dispose() {
			}

			public boolean isLabelProperty(Object element, String property) {
				return false;
			}

			public void removeListener(ILabelProviderListener listener) {
			}
			
		});
		fContentProvider= new TextSearchTableContentProvider();
		viewer.setContentProvider(fContentProvider);

		
		
	}
	
	protected void showMatch(Match match, int currentOffset, int currentLength, boolean activate) throws PartInitException {
		IEditorPart editor= null;
		IFile file = (IFile)match.getElement();
		try {
			//editor= EditorUtility.openInEditor(file, false);
		    editor = IDE.openEditor(PDTPlugin.getDefault().getActivePage(),file);
		} catch (PartInitException e1) {
			return;
		}

		if (editor != null && activate)
			editor.getEditorSite().getPage().activate(editor);
		if (editor instanceof ITextEditor) {
			ITextEditor textEditor= (ITextEditor) editor;
			textEditor.selectAndReveal(currentOffset, currentLength);
		}
	}

	
}