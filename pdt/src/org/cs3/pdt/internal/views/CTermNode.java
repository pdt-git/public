package org.cs3.pdt.internal.views;

import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CTerm;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.views.properties.IPropertySource;

public class CTermNode implements IAdaptable,Positional{

	public CTerm term;

	public CTermNode(CTerm term) {
		this.term=term;
	}

	public Object getAdapter(Class adapter) {
		if(IPropertySource.class.isAssignableFrom(adapter)){
			return new CTermPropertySource(term);
		}
		return null;
	}

	public String getPositionString() {
		return term.getAnotation("n").getFunctorValue();
	}
	public int[] getPositions() {
		String[] strings = Util.split(getPositionString(), ",");
		int[] ints = new int[strings.length];
		for (int i = 0; i < ints.length; i++) {
			ints[i]=Integer.parseInt(strings[i]);
		}
		return ints;
	}

}
