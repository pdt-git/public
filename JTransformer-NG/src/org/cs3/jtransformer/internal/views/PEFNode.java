/*
 */
package org.cs3.jtransformer.internal.views;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.model.IWorkbenchAdapter;


/**
 */
public class PEFNode implements IPEFNode,IAdaptable,/*IPropertySource,*/IWorkbenchAdapter {


    /**
     * Map containing mapping between Names and IPEFArguments
     */
    
    protected Map args;
    
    /**
     * Ordered list with the names of all arguments.
     */
    
    protected List argNames;
   
    protected List children;
    
	protected PrologInterface pef;
	
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
    public PEFNode(IViewSite site, String label, Map argMap, List argNames, PEFNode node,
    		PrologInterface pef) {
        this.args = argMap;
        this.argNames = argNames;
        this.label = label;
        this.site = site;
        this.parent = node;
    	this.pef = pef;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#getId()
     */
    public String getId() {
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
    public List getChildren() throws PrologInterfaceException {
       if(children==null){
           children=generateChildren();
       }
        return children;
    }

    public static IPEFNode find(IViewSite site, PEFNode parent, String id, String kind, PrologInterface pef) throws PrologInterfaceException{
    	if(id == null || id.equals("null"))
    		return null;
    		List errors = new ArrayList();
    		PrologSession session = null;
    		PEFNode node = null;
    		try {
    			session = pef.getSession();
	        	Map result = 
	        	session.queryOnce("pef_and_spec("+id+",Functor,Args,ArgDescrs,Term)");
	        	if(result == null || result.size() != 4){ 
	        		setStatusErrorMessage(errors,site, "error retrieving pef for id " +  id);
	        		return null;
	        	}
	        	List argArray= (List)result.get("Args");
	        	List argDescrs = (List)result.get("ArgDescrs");
	        	Map argMap = new HashMap();
	        	List argNames = new ArrayList();
	        	for (int i = 0; i < argArray.size(); i++) {
	        		List argDescr = (List)argDescrs.get(i);
	        		if(argDescr.get(ISVAR).toString().equals("yes"))
	        			setStatusErrorMessage(errors, site,"argument number  " +i  + " of "+ id + " is a variable!");
	
	        		else 
	        		    argMap.put(argDescr.get(ARGNAME), new PEFArgument(argArray.get(i),(String)argDescr.get(KIND),(String)argDescr.get(ARGNAME),argDescr.get(ISLIST).equals("list")));
	        		argNames.add(argDescr.get(ARGNAME));
				}
	        	List errs = session.queryAll("violatedContraints("+id+", Error)");
	        	for (Iterator iter = errs.iterator(); iter.hasNext();) {
					Map err = (Map) iter.next();
	        		setStatusErrorMessage(errors, site,(String)err.get("Error"));
	        	}
				String labelPrefix = "";
				if(kind != null && 
			     !(parent != null && parent.isList))
					labelPrefix = kind.toUpperCase() + ": ";
	        	node = new PEFNode(site, labelPrefix+result.get("Term"), argMap,argNames, parent,pef);
	        	node.setErrors(errors);
    		}catch(Exception ex) {
    			ex.printStackTrace();
    			throw new RuntimeException(ex);
    		} finally {
    			if (session != null)
    				session.dispose();
    		}
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
	 * @throws PrologInterfaceException 
     */
    protected List generateChildren() throws PrologInterfaceException {
		List list = new ArrayList();
		for (Iterator iter = argNames.iterator(); iter.hasNext();) {
			String name = (String) iter.next();
//			if (!(name.equals("parent") || name.equals("id") || name
//					.equals("encl"))) {
			if (!(name.equals("id"))) {
				IPEFArgument arg = (IPEFArgument) args.get(name);
				if (arg.getKind().equals("id"))
					if (!arg.isList()) {
						IPEFNode node = find(site, this, (String) arg.getArg(),arg.getName(),pef);
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
		List ids = (List) arg.getArg();
		if (ids.size() > 0) {

			Map argMap = new HashMap();
			List argNames = new ArrayList();
			argMap.put("id", new PEFArgument(getId()
					+ name, "id", "id", false));
			argMap.put("parent", this);
			for (int i = 0; i < ids.size(); i++) {
				argMap.put(name+i, new PEFArgument(ids.get(i), arg.getKind(), name +i, false));
				argNames.add(name+i);
			}
			PEFNode node = new PEFNode(site, name,
					argMap, argNames, this,pef);
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
//        if (IPropertySource.class.equals(adapter)){
//            return this;
//        }
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
    	//return null;
    	if(errors.isEmpty())
    	    return PlatformUI.getWorkbench().getSharedImages().
    		getImageDescriptor(ISharedImages.IMG_OBJS_TASK_TSK);
    	return PlatformUI.getWorkbench().getSharedImages().
		getImageDescriptor(ISharedImages.IMG_OBJS_ERROR_TSK);//ImageRepository.getImage(ImageRepository.PE_HIDDEN);
	    
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

	public PrologInterface getPrologInterface()
	{
		return pef;
	}
}
