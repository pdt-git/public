/*
 */
package org.cs3.pdt.internal.views;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pl.common.Util;
import org.cs3.pl.model.Node;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.PropertyDescriptor;

/**
 */
public class PrologNode extends Node implements IAdaptable,IPropertySource,IWorkbenchAdapter {

    private IPropertyDescriptor[] descriptors;
    /**
     * @param pif
     * @param Id
     * @param properties
     */
    public PrologNode(PrologInterface pif, String Id, Map properties) {
        super(pif, Id, properties);
    }
    /**
     * @return
     */
    protected List generateChildren() {        
        return find(pif,"parent("+id+")");
    }
    public static List find(PrologInterface pif, String properties){
        Vector result = new Vector();
        PrologSession s = pif.getSession();
        if(!properties.trim().startsWith("[")){
            properties="["+properties+"]";
        }
        try{
            //List r = s.queryAll("node(Id,"+properties+"),node(Id,List)");
        	List r = s.queryAll("node(Id,"+properties+"),node(Id,_List),parse_property_list(_List,Parsed)");
            for (Iterator iter = r.iterator(); iter.hasNext();) {
                Map ri = (Map) iter.next();
                String childId=(String) ri.get("Id");                
                List rr = (List) ri.get("Parsed");                
                Map childProperties = Util.parseAssociation(rr);
                result.add(new PrologNode(pif,childId,childProperties));
            }            
        }
        finally{
            s.dispose();
        }
        return result;
    }
    
    /* (non-Javadoc)
     * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
     */
    public Object getAdapter(Class adapter) {
        if (IPropertySource.class.equals(adapter)){
            return this;
        }
        if(IWorkbenchAdapter.class.equals(adapter)){
            return this;
        }
        return null;
    }
    /* (non-Javadoc)
     * @see org.eclipse.ui.views.properties.IPropertySource#getEditableValue()
     */
    public Object getEditableValue() {
        return this;
    }
    /* (non-Javadoc)
     * @see org.eclipse.ui.views.properties.IPropertySource#getPropertyDescriptors()
     */
    public IPropertyDescriptor[] getPropertyDescriptors() {
        if(descriptors==null){
            Iterator it = getProperties().keySet().iterator();
            descriptors=new IPropertyDescriptor[getProperties().size()];
            for (int i = 0; i < descriptors.length; i++) {
                String prop= (String) it.next();                
                descriptors[i]=new PropertyDescriptor(prop,prop);
            }
        }
        return descriptors;
    }
    /* (non-Javadoc)
     * @see org.eclipse.ui.views.properties.IPropertySource#getPropertyValue(java.lang.Object)
     */
    public Object getPropertyValue(Object id) {
        return getProperties().get(id);
    }
    /* (non-Javadoc)
     * @see org.eclipse.ui.views.properties.IPropertySource#isPropertySet(java.lang.Object)
     */
    public boolean isPropertySet(Object id) {
        return true;
    }
    /* (non-Javadoc)
     * @see org.eclipse.ui.views.properties.IPropertySource#resetPropertyValue(java.lang.Object)
     */
    public void resetPropertyValue(Object id) {
        ;
    }
    /* (non-Javadoc)
     * @see org.eclipse.ui.views.properties.IPropertySource#setPropertyValue(java.lang.Object, java.lang.Object)
     */
    public void setPropertyValue(Object id, Object value) {
        ;
    }
    
    
    /* (non-Javadoc)
     * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
     */
    public Object[] getChildren(Object parent) {        
        return getChildren().toArray();
    }
    /* (non-Javadoc)
     * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
     */
    public ImageDescriptor getImageDescriptor(Object object) {      
	        Object type = getProperties().get("type");
	        boolean exported = Boolean.valueOf((String) getProperties().get("exported")).booleanValue();
	        if("module".equals(type)){
	            return ImageRepository.getImageDescriptor(ImageRepository.PE_MODULE);
	        }
	        else if("predicate".equals(type)){
	            return ImageRepository.getImageDescriptor(exported? ImageRepository.PE_PUBLIC:ImageRepository.PE_HIDDEN);
	        }
	        if("clause".equals(type)){
	            return ImageRepository.getImageDescriptor(ImageRepository.PE_CLAUSE);
	        }	       
	    
		String imageKey = ISharedImages.IMG_OBJ_ELEMENT;	
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(imageKey);
	
    }
    /* (non-Javadoc)
     * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
     */
    public String getLabel(Object o) {
        return getId();
    }
    /* (non-Javadoc)
     * @see org.eclipse.ui.model.IWorkbenchAdapter#getParent(java.lang.Object)
     */
    public Object getParent(Object o) {
        return getParent();
    }

}
