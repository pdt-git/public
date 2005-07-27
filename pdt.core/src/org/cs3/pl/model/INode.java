/*
 */
package org.cs3.pl.model;

import java.util.List;
import java.util.Map;

/**
 */
public interface INode {
    public String getId();
    public String getParent();
    public List getChildren();
    public Map getProperties();
    public boolean hasChildren();
}
