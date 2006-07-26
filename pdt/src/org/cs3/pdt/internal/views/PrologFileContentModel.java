package org.cs3.pdt.internal.views;

import java.io.File;
import java.io.IOException;

import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;

public interface PrologFileContentModel {

	public abstract boolean hasChildren(Object parentElement);

	public abstract Object[] getChildren(Object parentElement)
			throws PrologInterfaceException;

	public abstract File getFile();

	public abstract void setFile(File file) throws 	IOException;

	public abstract void setPif(PrologInterface pif);

	public abstract PrologInterface getPif();

	public abstract void reset();

	public void addPrologFileContentModelListener(PrologFileContentModelListener l);
	public void removePrologFileContentModelListener(PrologFileContentModelListener l);

	public abstract void setRoot(Object input);
}