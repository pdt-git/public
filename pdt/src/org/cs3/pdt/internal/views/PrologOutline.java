/*
 * Created on 31.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.internal.views;


import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Predicate;
import org.eclipse.jface.viewers.IElementComparer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.texteditor.AbstractTextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

public class PrologOutline extends ContentOutlinePage {
    private final class Comparer implements IElementComparer {
		public int hashCode(Object element) {
			if(element instanceof Predicate){
				Predicate p = (Predicate) element;
				return p.getSignature().hashCode();
			}
			return element.hashCode();
		}

		public boolean equals(Object a, Object b) {
			if(a instanceof Predicate && b instanceof Predicate){
				Predicate pa = (Predicate) a;
				Predicate pb = (Predicate) b;
				return pa.getSignature().equals(pb.getSignature());
			}
			return a.equals(b);
		}
	}

    
    
    
	IEditorInput input;

    private AbstractTextEditor editor;

    private IDocumentProvider documentProvider;

    //	private TreeViewer viewer;
    private PrologElementContentProvider contentProvider;

    /**
     * @param provider
     * @param editor2
     */
    public PrologOutline(IDocumentProvider provider, AbstractTextEditor editor) {

        this.editor = editor;
        this.documentProvider = provider;
    }

    public void createControl(Composite parent) {
        super.createControl(parent);

        TreeViewer viewer = getTreeViewer();

        contentProvider = new PrologElementContentProvider(viewer);
        viewer.setContentProvider(contentProvider);
        viewer.setLabelProvider(new PrologElementLabelProvider());
        viewer.setComparer(new Comparer());
        viewer.setSorter(new ViewerSorter());
        viewer.addSelectionChangedListener(this);
        if (input != null)
            viewer.setInput(input);

;

    }


    public TreeViewer getTreeViewer() {
        return super.getTreeViewer();
    }

    public void setInput(IEditorInput input) {
        this.input = input;
        TreeViewer viewer = getTreeViewer();
        if(viewer!=null){
            viewer.setInput(input);
        }
    }

    public IEditorInput getInput() {
        return input;
    }

    /**
     * @param input2
     * @param b
     * @return
     */

    public void selectionChanged(SelectionChangedEvent event) {
        if (!((StructuredSelection) event.getSelection()).isEmpty()) {
			Object elm = ((StructuredSelection) event
                    .getSelection()).getFirstElement();
			
			Clause clause= null;
			if(elm instanceof Clause){
				clause = (Clause) elm;
			}
			else if(elm instanceof Predicate){
				Object[] children = getContentProvider().getChildren(elm);
				if (children==null || children.length==0){
					return;
				}
				clause=(Clause) children[0];
			}
			
            int pos = clause.getKnownDefinition().offset;
            int len = clause.getKnownDefinition().endOffset-pos;
            ((PLEditor) UIUtils.getActiveEditor())
                    .selectAndReveal(pos, len);
        }
    }

    public PrologElementContentProvider getContentProvider() {
        return contentProvider;
    }
}