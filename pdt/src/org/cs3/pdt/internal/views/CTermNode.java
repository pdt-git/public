package org.cs3.pdt.internal.views;

import org.cs3.pl.cterm.CTerm;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.views.properties.IPropertySource;

public class CTermNode implements IAdaptable{

	public CTerm term;

	public CTermNode(CTerm term) {
		this.term=term;
	}

	public Object getAdapter(Class adapter) {
		if(IPropertySource.class.isAssignableFrom(adapter)){
			System.out.println("yep");
			return new CTermPropertySource(term);
		}
		return null;
	}

}
