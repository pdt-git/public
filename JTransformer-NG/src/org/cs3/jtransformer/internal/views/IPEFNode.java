/*
 * Created on 15.01.2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.jtransformer.internal.views;

import java.util.List;

/**
 */
public interface IPEFNode {
    public String getId();
    public String getParent();
    public List getChildren();
    public boolean hasChildren();
}
