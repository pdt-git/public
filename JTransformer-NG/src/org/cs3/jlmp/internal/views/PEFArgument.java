/*
 * Created on 15.01.2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.jlmp.internal.views;

/**
 * @author rho
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class PEFArgument implements IPEFArgument {

	private Object argument;
	private String kind;
	private String name;
	private boolean isList;

	public PEFArgument(Object argument, String kind,String name, boolean isList) {
		this.argument = argument;
		this.kind = kind;
		this.name = name;
		this.isList = isList;
		if(isList && argument.equals("[]"))
			this.argument = new Object[0];
	}
	
	/* (non-Javadoc)
	 * @see org.cs3.pdt.internal.views.IPEFArgument#getArg()
	 */
	public Object getArg() {
		return argument;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pdt.internal.views.IPEFArgument#getKind()
	 */
	public String getKind() {
		// TODO Auto-generated method stub
		return kind;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pdt.internal.views.IPEFArgument#getName()
	 */
	public String getName() {
		// TODO Auto-generated method stub
		return name;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pdt.internal.views.IPEFArgument#isList()
	 */
	public boolean isList() {
		// TODO Auto-generated method stub
		return isList;
	}

}
