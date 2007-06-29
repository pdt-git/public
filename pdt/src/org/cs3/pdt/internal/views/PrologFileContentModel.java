package org.cs3.pdt.internal.views;

import java.io.File;

import org.cs3.pl.prolog.IPrologEventDispatcher;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;

public interface PrologFileContentModel {

	public abstract boolean hasChildren(Object parentElement);

	public abstract Object[] getChildren(Object parentElement)
			throws PrologInterfaceException;

	
	public abstract File getFile();

	

	public abstract void setPif(PrologInterface pif,IPrologEventDispatcher d) throws PrologInterfaceException;

	public abstract PrologInterface getPif();

	public abstract void reset() throws PrologInterfaceException;

	public void addPrologFileContentModelListener(PrologFileContentModelListener l);
	public void removePrologFileContentModelListener(PrologFileContentModelListener l);
	
	public void addPrologFileContentModelListener(Object parent,PrologFileContentModelListener l);
	public void removePrologFileContentModelListener(Object parent,PrologFileContentModelListener l);

	public abstract void setInput(Object input);
	public abstract Object getInput();

	public abstract void dispose();

	public abstract long getLastResetTime();
}