/*
 */
package org.cs3.pdt.internal.views;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pl.ImageRepository;
import org.cs3.pl.prolog.IPrologClient;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;

/**
 */
public class PEFNode implements INode,IAdaptable,IPropertySource,IWorkbenchAdapter {

    protected IPrologClient pif;

    protected List args;
    protected List children;
	private static final int ISVAR = 4;
	private static final int ARGNAME = 0;
	private static final int KIND = 1;
	private Map propertyKinds;
	private static final int ISLIST = 2;
	private String label;
	private PEFNode parent;

    /**
     * @param string
     * @param kindMap
     * 
     */
    public PEFNode(IPrologClient pif, String label, PEFNode parent, List args) {
        this.pif=pif;
        this.args = args;
        this.label = label;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#getId()
     */
    public String getId() {
        return (String)((IPEFArgument)args.get(0)).getArg();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#getParent()
     */
    public String getParent() {
    	IPEFArgument pef = (IPEFArgument)args.get(1);
    	if(pef.getName().equals("parent"))
    		return (String)pef.getArg();
    	else 
    		return null;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#getChildren()
     */
    public List getChildren() {
       if(children==null){
           children=generateChildren();
       }
        return children;
    }

    public static INode find(IPrologClient client, PEFNode parent, String id){
    	if(id == null || id.equals("null"))
    		return null;
        	Hashtable result = 
        	client.query("pef_and_spec("+id+",Functor,Args,ArgDescrs,Term)");
        	if(result == null || result.size() != 5) 
        		throw new RuntimeException("error retrieving pef for id " +  id);
        	Object[] args= (Object[])result.get("Args");
        	Object[] argDescrs = (Object[])result.get("ArgDescrs");
        	List argList = new ArrayList();
        	for (int i = 0; i < args.length; i++) {
        		Object[] argDescr = (Object[])argDescrs[i];
        		if(argDescr[ISVAR].toString().equals("yes"))
            		System.err.println("argument number  " +i  + " of "+ id + " is a variable!");
        		else 
        		argList.add(new PEFArgument(args[i],(String)argDescr[KIND],(String)argDescr[ARGNAME],argDescr[ISLIST].equals("list")));
        		
			}
        	return new PEFNode(client, (String)result.get("Term"), parent,argList);
       }

    /**
     * @return
     */
    protected List generateChildren() {
    	List list = new ArrayList();
    	for (Iterator iter = args.iterator(); iter.hasNext();) {
    		IPEFArgument element = (IPEFArgument) iter.next();
			if(element.getKind().equals("id") && !
					(element.getName().equals("parent") ||
					 element.getName().equals("id") ||
					 element.getName().equals("encl")))
	    		if(!element.isList()) {
	    			INode node = find(pif, this, (String) element.getArg());
					if (node != null)
						list.add(node);
				} else {
					Object[] ids = (Object[]) element.getArg();
					if (ids.length > 0) {

						List argList = new ArrayList();
						argList.add(new PEFArgument(getId() + element.getName(), "id","id", false));
						for (int i = 0; i < ids.length; i++) {
							argList.add(new PEFArgument(ids[i], element
									.getKind(), element.getName(), false));
						}
						list.add(new PEFNode(pif, element.getName(), this, argList));
					}
	    		}
    		

		}
        return list;
    }



    /* (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        if(!(obj instanceof PEFNode)){
            return false;
        }
        return ((PEFNode)obj).getId().equals(getId());
    }
    
    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return getId().hashCode();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#hasChildren()
     */
    public boolean hasChildren() {
        
        return args.size() > 0;
    }

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
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.IPropertySource#getPropertyDescriptors()
	 */
	public IPropertyDescriptor[] getPropertyDescriptors() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.IPropertySource#getPropertyValue(java.lang.Object)
	 */
	public Object getPropertyValue(Object id) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.IPropertySource#isPropertySet(java.lang.Object)
	 */
	public boolean isPropertySet(Object id) {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.IPropertySource#resetPropertyValue(java.lang.Object)
	 */
	public void resetPropertyValue(Object id) {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.IPropertySource#setPropertyValue(java.lang.Object, java.lang.Object)
	 */
	public void setPropertyValue(Object id, Object value) {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
	 */
	public Object[] getChildren(Object o) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
     * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
     */
    public ImageDescriptor getImageDescriptor(Object object) {      
	    return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
	    
    }
	/* (non-Javadoc)
	 * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
	 */
	public String getLabel(Object o) {
		// TODO Auto-generated method stub
		return label;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.model.IWorkbenchAdapter#getParent(java.lang.Object)
	 */
	public Object getParent(Object o) {
		// TODO Auto-generated method stub
		return parent;
	}
}
