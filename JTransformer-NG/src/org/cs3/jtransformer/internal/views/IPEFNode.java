/*
 * Created on 15.01.2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.jtransformer.internal.views;

import java.util.List;

import org.cs3.pl.prolog.PrologInterfaceException;

/**
 */
public interface IPEFNode {
    public String getId();
    public String getParent();
    public List getChildren() throws PrologInterfaceException;
    public boolean hasChildren();
}
