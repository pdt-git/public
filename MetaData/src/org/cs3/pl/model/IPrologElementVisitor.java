/*
 */
package org.cs3.pl.model;

import java.util.List;

/**
 * an object that traverses the prolog model
 */
public interface IPrologElementVisitor {
    /**
     * @param node The visited node.
     * @param path The path containing all nodes already visited.
     * @return true if adjacents of node should be visited.<p>
     * <b>NOTE: the code of the individual nodes will make sure that
     * cycles are broken. No node will be visited twice.</b>
     */
    boolean visit(IPrologElement node, List path, Object role);
}
