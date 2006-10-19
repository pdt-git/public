package salvo.jesus.graph.listener;
import salvo.jesus.graph.*;
import salvo.jesus.graph.algorithm.*;
import java.util.*;

/**
 * ConnectedSetListener maintains connected set information about the graph on
 * which it is listening.
 *
 * @author John V. Sichi
 * @version $Id$
 */
public class ConnectedSetListener extends NullGraphListener
{
    private Graph m_graph;

    /**
     * List< Set<Vertex> > where each Set is a connected set.  The assumption
     * is that a typical graph will not have very many connected sets, so a
     * List is lower-overhead than a Map.  Consider making a Map implementation
     * which can dynamically select between a List-based implementation and a
     * HashMap-based implementation.
     */
    private List m_connectedSetList;

    /**
     * Construct a new ConnectedSetListener, building sets based on any
     * existing graph components.
     *
     * @param graph the graph on which to construct the listener;
     * this constructor will automatically register the new listener
     * with the graph
     */
    public ConnectedSetListener(Graph graph)
    {
        m_graph = graph;
        m_connectedSetList = new ArrayList();
        // Add information about anything which is already part of the graph
        // This can be done more efficiently than the incremental methods by
        // using repeated DepthFirstGraphTraversals.  Note that even if the
        // graph is directed, we still use an undirected DFS, because the
        // connected set definition is direction-independent.
        GraphTraversal traversal = new DepthFirstGraphTraversal(m_graph);
        Iterator vertexIter = m_graph.getVerticesIterator();
        while (vertexIter.hasNext()) {
            Vertex v = (Vertex) vertexIter.next();
            Set connectedSet = getModifiableConnectedSet(v);
            if (connectedSet != null) {
                // We've already seen this vertex, so it and everything
                // reachable from it are already in an existing connected set.
                // Skip it.
                continue;
            }
            connectedSet = new HashSet(traversal.traverse(v));
            m_connectedSetList.add(connectedSet);
        }
        m_graph.addListener(this);
    }

    /**
     * Get the connected set to which the given vertex belongs.
     *
     * @param v the vertex to search for
     *
     * @return an unmodifiable Set<Vertex>
     */
    public Set getConnectedSet(Vertex v)
    {
        return Collections.unmodifiableSet(
            getModifiableConnectedSet(v));
    }

    /**
     * Get a collection of all connected sets for this graph.
     *
     * @return Collection< Set<Vertex> >
     */
    public Collection getConnectedSets()
    {
        Collection c = new ArrayList();
        Iterator iter = m_connectedSetList.iterator();
        while (iter.hasNext()) {
            Set s = (Set) iter.next();
            c.add(Collections.unmodifiableSet(s));
        }
        return Collections.unmodifiableCollection(c);
    }
    
    private Set getModifiableConnectedSet(Vertex v)
    {
        for (int i = 0; i < m_connectedSetList.size(); i++) {
            Set set = (Set) m_connectedSetList.get(i);
            if (set.contains(v)) {
                return set;
            }
        }
        return null;
    }

    // override the after events rather than the before events because
    // we don't want to update our state until we're sure the
    // graph actually changed
    
    public void afterEdgeAdded(GraphAddEdgeEvent event)
    {
        // if the incident vertices are from different connected sets, then
        // merge the two connected sets
        Edge edge = event.getEdge();
        Set setA = getModifiableConnectedSet(edge.getVertexA());
        Set setB = getModifiableConnectedSet(edge.getVertexB());
        if (setA == setB) {
            return;
        }

        // merge the smaller set into the larger one
        if (setA.size() < setB.size()) {
            setB.addAll(setA);
            m_connectedSetList.remove(setA);
        } else {
            setA.addAll(setB);
            m_connectedSetList.remove(setB);
        }
    }

    public void afterVertexAdded(GraphAddVertexEvent event)
    {
        Vertex v = event.getVertex();
        // create a new, isolated set for the vertex
        Set newSet = new HashSet();
        newSet.add(v);
        m_connectedSetList.add(newSet);
    }

    public void afterEdgeRemoved(GraphRemoveEdgeEvent event)
    {
        Edge edge = event.getEdge();
        Vertex v1 = edge.getVertexA();
        Vertex v2 = edge.getVertexB();
        // Determine if we need to unmerge a connected set.  If there is no
        // path from fromvertex to tovertex...
        StopAtVisitor visitor = new StopAtVisitor(v1);
        GraphTraversal traversal = new DepthFirstGraphTraversal(m_graph);
        List v2ConnectedList = traversal.traverse(v2,visitor);
        if (visitor.wasFound()) {
            // still connected
            return;
        }
        // ... unmerge the connected sets. Do this by creating a new
        // connected set based on the list we just found starting with v2
        // ....
        Set v1ConnectedSet = getModifiableConnectedSet(v1);
        Set v2ConnectedSet = new HashSet(v2ConnectedList);

        // ... and removing elements from the existing set that are in the
        // new set...
        v1ConnectedSet.removeAll(v2ConnectedSet);

        // .. and finally adding the new set to the set of connected sets.
        m_connectedSetList.add(v2ConnectedSet);
    }

    public void afterVertexRemoved(GraphRemoveVertexEvent event)
    {
        Vertex v = event.getVertex();
        // When a vertex is removed, it's guaranteed that it wasn't connected
        // to anything, so we don't have to worry about removing it from its
        // connected set; the connected set can just go away with it.
        m_connectedSetList.remove(getModifiableConnectedSet(v));
    }
}

// End ConnectedSetListener.java
