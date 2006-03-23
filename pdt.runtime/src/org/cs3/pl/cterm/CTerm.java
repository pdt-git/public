package org.cs3.pl.cterm;

import java.util.Map;



public interface CTerm {
	boolean hasAnnotation(String string);
	public CTerm getAnotation(String functor);
	public String getFunctorImage();
	public int getArity();
	public String getFunctorValue();
	public CTermFactory getFactory();
}
