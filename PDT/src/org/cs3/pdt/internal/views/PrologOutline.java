/*
 * Created on 31.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.internal.views;

import java.util.ArrayList;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.PrologElement;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.PrologElementData;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.ConsultServiceEvent;
import org.cs3.pl.prolog.ConsultServiceListener;
import org.cs3.pl.prolog.PrologException;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.texteditor.AbstractTextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

public class PrologOutline extends ContentOutlinePage {
    private IEditorInput input;

    private AbstractTextEditor editor;

    private IDocumentProvider documentProvider;

    //	private TreeViewer viewer;
    private ContentProvider contentProvider;

    public  class ContentProvider implements ITreeContentProvider, ConsultServiceListener {

        private PrologElement[] predicates = new PrologElement[0];

        private IDocumentProvider documentProvider;

        /**
         * @param filtering
         */
        public ContentProvider(IDocumentProvider documentProvider) {
            this.documentProvider = documentProvider;
            ConsultService service = PDTPlugin.getDefault().getConsultService(PDT.CS_METADATA);
            service.addConsultServiceListener(this);
        }

        /**
         * @see org.eclipse.jface.viewers.IContentProvider#dispose()
         */
        public void dispose() {
        }

        /**
       
         * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(Viewer,
         *           Object, Object)
         */
        public void inputChanged(final Viewer viewer, Object oldInput,
                Object input) {

            try {
                IFileEditorInput editorInput = null;
                if (input == null) {
                    return;
                }
                try {
                    editorInput = (IFileEditorInput) input;
                } catch (ClassCastException e) {
                    //not our concern.
                    return;
                }
                IFile file = editorInput.getFile();
                String fileName = file.getFullPath().toString();
                generatePredicates(fileName);
                return;
            } catch (Exception e) {
                Debug.report(e);
            }
        }

        /**
         * @param fileName
         * @throws PrologException
         */
        private void generatePredicates(String fileName) throws PrologException {
            PDTPlugin plugin = PDTPlugin.getDefault();                
            IMetaInfoProvider metaInfo = plugin.getMetaInfoProvider();
            PrologElementData[] data = metaInfo.retrievePrologElements(fileName);
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
            /*
             * for (int i = l.size() - 1; i >= 0; i--) l2.add(l.get(i));
             */
            for (int i = 0; i < l.size(); i++){
                l2.add(l.get(i));
            }
            predicates = PrologElement.fromData((PrologElementData[]) l2
                    .toArray(new PrologElementData[0]));
            return;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
         */
        public Object[] getChildren(Object parentElement) {
            return predicates;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
         */
        public Object getParent(Object element) {
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
         */
        public boolean hasChildren(Object element) {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
         */
        public Object[] getElements(Object inputElement) {
            return predicates;
        }

        /* (non-Javadoc)
         * @see org.cs3.pl.metadata.ConsultServiceListener#consultDataChanged(org.cs3.pl.metadata.ConsultServiceEvent)
         */
        public void consultDataChanged(final ConsultServiceEvent e) {
            TreeViewer viewer = getTreeViewer();
            if(viewer==null||viewer.getControl().isDisposed()){
                ConsultService service = PDTPlugin.getDefault().getConsultService(PDT.CS_METADATA);
                service.removeConsultServiceListener(this);
                return;
            }
            Display display = viewer.getControl().getDisplay();
            if(Display.getCurrent()!=display){
                display.asyncExec(new Runnable(){
                    public void run() {
                        consultDataChanged(e);
                    }
                });
                return;
            }
            IFileEditorInput editorInput;
            try {
                editorInput = (IFileEditorInput) input;
            } catch (ClassCastException cce) {
                //not our concern.
                return;
            }
            IFile file = editorInput.getFile();
            String fileName = file.getFullPath().toString();
            if(e.getSymbol()==null||e.getSymbol().equals(fileName)){
                try {
                    generatePredicates(fileName);
                } catch (PrologException e1) {
                    Debug.report(e1);
                }
                viewer.refresh();
            }            
        }

    }

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

        contentProvider = new ContentProvider(documentProvider);
        viewer.setContentProvider(contentProvider);
        viewer.setLabelProvider(new WorkbenchLabelProvider());

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
            PrologElement element = ((PrologElement) ((StructuredSelection) event
                    .getSelection()).getFirstElement());
            int pos = element.getPosition();
            int len = element.getLength();
            ((PLEditor) PDTPlugin.getDefault().getActiveEditor())
                    .selectAndReveal(pos, len);
        }
    }

    public ContentProvider getContentProvider() {
        return contentProvider;
    }
}