/*
 * Created on 15.01.2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pdt.internal.views;

import java.util.List;
import java.util.Map;

/**
 */
public interface INode {
    public String getId();
    public String getParent();
    public List getChildren();
    public boolean hasChildren();
}
