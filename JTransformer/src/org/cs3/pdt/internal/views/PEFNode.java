/*
 */
package org.cs3.pdt.internal.views;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pl.ImageRepository;
import org.cs3.pl.prolog.IPrologClient;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;

/**
 */
public class PEFNode implements INode,IAdaptable,IPropertySource,IWorkbenchAdapter {

    protected IPrologClient pif;

    /**
     * Map containing mapping between Names and IPEFArguments
     */
    
    protected Map args;
    
    /**
     * Ordered list with the names of all arguments.
     */
    
    protected List argNames;
    
    protected List children;
	private static final int ISVAR = 4;
	private static final int ARGNAME = 0;
	private static final int KIND = 1;
	private Map propertyKinds;
	private static final int ISLIST = 2;
	private String label;
	private PEFNode parent;
	private List errors = new ArrayList();

	private IViewSite site;

	private boolean isList = false; 

    /**
     * @param erroneous2
     * @param string
     * @param kindMap
     * 
     */
    public PEFNode(IViewSite site, IPrologClient pif, String label, Map argMap, List argNames, PEFNode node) {
        this.pif=pif;
        this.args = argMap;
        this.argNames = argNames;
        this.label = label;
        this.site = site;
        this.parent = node;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#getId()
     */
    public String getId() {
    	if(args.get("id") == null)
    		System.out.println("A");
        return (String)((IPEFArgument)args.get("id")).getArg();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#getParent()
     */
    public String getParent() {
    	IPEFArgument pef = (IPEFArgument)args.get("parent");
    	if(pef != null)
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

    public static INode find(IViewSite site, IPrologClient client, PEFNode parent, String id){
    	if(id == null || id.equals("null"))
    		return null;
    		List errors = new ArrayList();
        	Hashtable result = 
        	client.query("pef_and_spec("+id+",Functor,Args,ArgDescrs,Term)");
        	if(result == null || result.size() != 5){ 
        		setStatusErrorMessage(errors,site, "error retrieving pef for id " +  id);
        		return null;
        	}
        	Object[] argArray= (Object[])result.get("Args");
        	Object[] argDescrs = (Object[])result.get("ArgDescrs");
        	Map argMap = new HashMap();
        	List argNames = new ArrayList();
        	for (int i = 0; i < argArray.length; i++) {
        		Object[] argDescr = (Object[])argDescrs[i];
        		if(argDescr[ISVAR].toString().equals("yes"))
        			setStatusErrorMessage(errors, site,"argument number  " +i  + " of "+ id + " is a variable!");

        		else 
        		    argMap.put(argDescr[ARGNAME], new PEFArgument(argArray[i],(String)argDescr[KIND],(String)argDescr[ARGNAME],argDescr[ISLIST].equals("list")));
        		argNames.add(argDescr[ARGNAME]);
			}
        	Hashtable err = client.query("violatedContraints("+id+", Error)");
        	while(err != null && err.size() > 0) {
        		setStatusErrorMessage(errors, site,(String)err.get("Error"));
        		err = client.next();
        	}
        	PEFNode node = new PEFNode(site, client, (String)result.get("Term"), argMap,argNames, parent);
        	node.setErrors(errors);
        	return node; 
       }

    /**
	 * @param errors2
	 */
	private void setErrors(List errors2) {
		if(errors2 != null)
		this.errors = errors2;
	}

	public List getErrors(){
		return errors;
	}
	public String getErrorMessages(){
		String msg = "";
		int i = 1;
		for (Iterator iter = errors.iterator(); iter.hasNext();){ 
			msg += "\n"+i++ + ". " +iter.next();	
		}
		return msg;
	}
	/**
	 * @param errors2
     * @param site
	 * @param string
	 */
	private static void setStatusErrorMessage(List errors, IViewSite site, String string) {
		site.getActionBars().getStatusLineManager().setErrorMessage(string);
		errors.add(string);
	}

	/**
     * @return
     */
    protected List generateChildren() {
		List list = new ArrayList();
		for (Iterator iter = argNames.iterator(); iter.hasNext();) {
			String name = (String) iter.next();
			if (!(name.equals("parent") || name.equals("id") || name
					.equals("encl"))) {
				IPEFArgument arg = (IPEFArgument) args.get(name);
				if (arg.getKind().equals("id"))
					if (!arg.isList()) {
						INode node = find(site, pif, this, (String) arg.getArg());
						if (node != null)
							list.add(node);
					} else {
						addListNode(list, name, arg);
					}
			}
		}
		return list;
	}



    /**
	 * @param list
	 * @param name
	 * @param arg
	 */
	private void addListNode(List list, String name, IPEFArgument arg) {
		Object[] ids = (Object[]) arg.getArg();
		if (ids.length > 0) {

			Map argMap = new HashMap();
			List argNames = new ArrayList();
			argMap.put("id", new PEFArgument(getId()
					+ name, "id", "id", false));
			argMap.put("parent", this);
			for (int i = 0; i < ids.length; i++) {
				argMap.put(name+i, new PEFArgument(ids[i], arg.getKind(), name +i, false));
				argNames.add(name+i);
			}
			PEFNode node = new PEFNode(site, pif, name,
					argMap, argNames, this);
			node.isList = true;
			list.add( node);
		}
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
    	if(errors.isEmpty())
    	    return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
    	return ImageRepository.getImage(ImageRepository.PE_HIDDEN);
	    
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

	/**
	 * @return
	 */
	public boolean isList() {
		return isList;
	}
}
