package salvo.jesus.graph;

import java.util.*;
import java.io.*;

/**
 * GraphListener is the listener interface for receiving notifications when a
 * Graph is modified in any way.  Implementations of this interface must be
 * registered with a Graph via the Graph's addListener() method in order to
 * receive notifications.
 *
 *<p>
 *
 * There are eight interface methods, representing combinations of
 * (before/after) (add/remove) (edge/vertex).  Note that a
 * single graph modification method call may result in multiple listener method
 * invocations.  For example, when an edge is added and an incident vertex
 * is not already part of the graph, the vertex will be added implicitly; this
 * results in a listener sequence like (beforeVertexAdded, afterVertexAdded,
 * beforeEdgeAdded, afterEdgeAdded).  Similarly, when a vertex is removed, all
 * incident edges are removed as well.  Listeners can detect compound
 * events because extra event fields are set for these cases with
 * information about the related object.
 *
 *<p>
 *
 * Listeners may throw exceptions from before-methods, but not after-methods.
 * An exception thrown from a before-method causes the intended operation to
 * fail without modifying the graph.  The reason that after-methods may not
 * throw exceptions is that by this time, the graph has already been modified,
 * so throwing an exception would be misleading, and might also interfere with
 * other listeners.  So to be safe, a before-method should do all
 * necessary validation on the event, and throw an exception if validation
 * fails, but should NOT update any of the listener's own state, since the
 * operation is not guaranteed to complete.  Likewise, an after-method should
 * update any listener state to reflect the modification which took place.
 *
 *<p>
 *
 * Note that in the case of compound modifications described previously, the
 * overall operation is guaranteed to be atomic, because all before-listeners
 * involved are invoked before any modification takes place.  For example,
 * suppose a call is made to remove a vertex which has one incident edge.
 * First the before-listeners for both vertex and edge removal are invoked,
 * and only then is the edge removed, and then the vertex.  So if any
 * before-listener throws an exception, the operation is aborted
 * with no work done.  Because of this ordering rule, before-listeners have
 * to be coded carefully; when beforeEdgeAdded is called, the incident
 * vertices might not yet have been added to the graph; likewise,
 * when beforeVertexRemoved is called, the incident edges might not
 * yet have been removed.
 *
 *<p>
 *
 * GraphListener is declared as Serializable, and when a graph is serialized,
 * any attached GraphListeners are serialized as well.  If serialization
 * of an attached listener fails, this will cause graph serialization to fail
 * as well, so care should be taken when developing listeners to mark fields as
 * transient where necessary.
 *
 *<p>
 *
 * The source for all events originating from graph modifications is the graph
 * itself.
 *
 *<p>
 *
 * To write a listener which is only interested in specific events, see
 * salvo.jesus.graph.listener.NullGraphListener.
 *
 * @author John V. Sichi
 * @version $Id$
 */
public interface GraphListener extends EventListener, Serializable
{
    /**
     * Called when a vertex is being added to a graph, just before the graph
     * structure is actually changed.
     *
     * @param event the event describing the vertex addition
     *
     * @exception the listener may throw any Exception, which will be
     * propagated to the code which invoked the original graph modification
     * method
     */
    public void beforeVertexAdded(GraphAddVertexEvent event)
        throws Exception;
    
    /**
     * Called when a vertex is being added to a graph, just after the graph
     * structure has actually been changed.
     *
     * @param event the event describing the vertex addition
     */
    public void afterVertexAdded(GraphAddVertexEvent event);
    
    /**
     * Called when a vertex is being removed from a graph, just before the
     * graph structure is actually changed.
     *
     * @param event the event describing the vertex removal
     *
     * @exception the listener may throw any Exception, which will be
     * propagated to the code which invoked the original graph modification
     * method
     */
    public void beforeVertexRemoved(GraphRemoveVertexEvent event)
        throws Exception;
    
    /**
     * Called when a vertex is being removed from a graph, just after the
     * graph structure has actually been changed.
     *
     * @param event the event describing the vertex removal
     */
    public void afterVertexRemoved(GraphRemoveVertexEvent event);
    
    /**
     * Called when an edge is being added to a graph, just before the graph
     * structure is actually changed.
     *
     * @param event the event describing the edge addition
     *
     * @exception the listener may throw any Exception, which will be
     * propagated to the code which invoked the original graph modification
     * method
     */
    public void beforeEdgeAdded(GraphAddEdgeEvent event)
        throws Exception;
    
    /**
     * Called when an edge is being added to a graph, just after the graph
     * structure has actually been changed.
     *
     * @param event the event describing the edge addition
     */
    public void afterEdgeAdded(GraphAddEdgeEvent event);

    /**
     * Called when an edge is being removed from a graph, just before the
     * graph structure is actually changed.
     *
     * @param event the event describing the edge removal
     *
     * @exception the listener may throw any Exception, which will be
     * propagated to the code which invoked the original graph modification
     * method
     */
    public void beforeEdgeRemoved(GraphRemoveEdgeEvent event)
        throws Exception;

    /**
     * Called when an edge is being removed from a graph, just after the
     * graph structure has actually been changed.
     *
     * @param event the event describing the edge removal
     */
    public void afterEdgeRemoved(GraphRemoveEdgeEvent event);
}

// End GraphListener.java
