/*
 */
package model;

import java.util.List;

/**
 * an object that traverses the prolog model
 */
public interface IPrologElementVisitor {
    /**
     * @param node The visited node.
     * @param path The path containing all nodes already visited.
     * @return true if adjacents of node should be visited.<p><b>NOTE: it is in the
     * responsibility of the visitor to check for cycles AND break them by returning
     * false when visiting a node twice.</b>
     */
    boolean visit(IPrologElement node, List path);
}
