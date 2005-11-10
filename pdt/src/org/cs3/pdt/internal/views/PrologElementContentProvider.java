/**
 * 
 */
package org.cs3.pdt.internal.views;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.ConsultServiceEvent;
import org.cs3.pl.prolog.ConsultServiceListener;
import org.cs3.pl.prolog.PrologException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IFileEditorInput;

public  class PrologElementContentProvider implements ITreeContentProvider, ConsultServiceListener {

    private final Viewer viewer;

	
	private HashMap data = new HashMap();


	private ConsultService service;
   

    /**
     * @param outline TODO
     * @param filtering
     */
    public PrologElementContentProvider(Viewer outline) {
        viewer = outline;
		
		
    }

    private void setConsultService(ConsultService nservice){
    	if(this.service!=null){
    		this.service.removeConsultServiceListener(this);
    	}
    	this.service=nservice;
    	if(this.service!=null){
    		this.service.addConsultServiceListener(this);
    	}
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
            
            try {
                editorInput = (IFileEditorInput) input;
            } catch (ClassCastException e) {            	
                
            }
            if (input == null) {
                setConsultService(null);
                return;
            }
            IFile file = editorInput.getFile();
            IPrologProject nature = (IPrologProject) file.getProject().getNature(PDTCore.NATURE_ID);
            setConsultService(nature.getMetadataPrologInterface().getConsultService(PDTCore.CS_METADATA));
            String fileName = file.getFullPath().toString();
            
            generatePredicates(nature.getMetaInfoProvider(),fileName);
            return;
        } catch (Exception e) {
            Debug.report(e);
        }
    }

    /**
     * @param provider 
     * @param fileName
     * @throws PrologException
     */
    private void generatePredicates(IMetaInfoProvider provider, String fileName) throws PrologException {
        
		data.clear();
        Clause[] clauses = provider.retrievePrologElements(fileName);
        if (clauses == null || clauses.length == 0) {            
            return;
        }
        for (int i = 0; i < clauses.length; i++) {
			Clause clause = clauses[i];
			Predicate p = clause.getPredicate();
			List l = (List) data.get(p);
			if(l==null){
				l=new ArrayList();
				data.put(p,l);
			}
			l.add(clause);
		}					
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
     */
    public Object[] getChildren(Object parentElement) {
		if(parentElement instanceof Predicate){
			List l = (List) data.get(parentElement);
			if (l==null){
				return new Object[0];
			}
			return l.toArray();
		}
		//otherwise we assume that parentElement is the invisible root node.
		
        return data.keySet().toArray();
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
        return element instanceof Predicate;
					
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
     */
    public Object[] getElements(Object inputElement) {
        return getChildren(inputElement);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.metadata.ConsultServiceListener#consultDataChanged(org.cs3.pl.metadata.ConsultServiceEvent)
     */
    public void consultDataChanged(final ConsultServiceEvent e) {
       
        if(viewer==null||viewer.getControl().isDisposed()){
            
            
			ConsultService service = (ConsultService)e.getSource(); 
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
            editorInput = (IFileEditorInput) viewer.getInput();
        } catch (ClassCastException cce) {
            //not our concern.
            return;
        }
        IFile file = editorInput.getFile();
        IPrologProject nature;
		try {
			nature = (IPrologProject) file.getProject().getNature(PDTCore.NATURE_ID);
		} catch (CoreException ee) {
			Debug.report(ee);
			throw new RuntimeException(ee);
		}
        
        String fileName = file.getFullPath().toString();
        //if(e.getSymbol()==null||e.getSymbol().equals(fileName)){
            try {
                generatePredicates(nature.getMetaInfoProvider(),fileName);
            } catch (PrologException e1) {
                Debug.report(e1);
            }
            viewer.refresh();
        //}            
    }

}