/*
 */
package org.cs3.pdt;

import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.SessionException;
import org.eclipse.core.resources.IFile;

/**
 */
public interface IPrologHelper extends IMetaInfoProvider{

    public void consult(IFile file) throws SessionException;
    public boolean assertFact(String fact) throws SessionException;
    public void showSourceLocation(SourceLocation loc);
}
