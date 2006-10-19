package salvo.jesus.graph.listener;
import salvo.jesus.graph.*;
import salvo.jesus.graph.algorithm.*;
import java.util.*;

/**
 * PathListener imposes a path structure on a Graph.
 * It can be used as a delegate by any class which wants to implement the Path
 * interface.
 *
 * @author John V. Sichi
 * @version $Id$
 */
public class PathListener
    extends NullGraphListener
    implements AbstractPathListener
{
    /**
     * Array of all the vertices in this Path, from first to last.  We use an
     * ArrayList to avoid the unneeded cost of Stack synchronization.
     */
    private List m_vertexList;

    /**
     * The graph on which we are listening.
     */
    private Path m_path;

    /**
     * Creates a new PathListener for the given graph.
     *
     * @param graph the graph to which this listener is to be attached;
     * this constructor will automatically register the listener
     * to receive all events
     *
     */
    public PathListener(Path path)
    {
        m_path = path;
        m_path.addListener(this);
        m_vertexList = new ArrayList();
        if (m_path.getVerticesCount() > 0) {
            m_vertexList.addAll(
                m_path.traverse(m_path.getFirstVertex()));
        }
    }

    /**
     * Returns the first <tt>Vertex</tt> in the <tt>Path</tt>.
     */
    public Vertex getFirstVertex() {
        if (m_vertexList.isEmpty()) {
            return null;
        }
        return (Vertex) m_vertexList.get(0);
    }

    /**
     * Returns the last <tt>Vertex</tt> in the <tt>Path</tt>.
     */
    public Vertex getLastVertex() {
        if (m_vertexList.isEmpty()) {
            return null;
        }
        return (Vertex) m_vertexList.get(m_vertexList.size()-1);
    }

    public void afterVertexAdded(GraphAddVertexEvent event)
    {
        if (m_vertexList.isEmpty()) {
            // adding our first vertex
            m_vertexList.add(event.getVertex());
        } else {
            // we must be adding an edge at the same time, so let the edge
            // listener handle it
        }
    }

    // NOTE:  don't need any beforeVertexRemoved() checking, because GraphImpl
    // always removes all edges first, which will catch an illegal vertex
    // removal also

    public void afterVertexRemoved(GraphRemoveVertexEvent event)
    {
        m_vertexList.remove(m_vertexList.size()-1);
    }

    public void beforeEdgeAdded(GraphAddEdgeEvent event)
        throws Exception
    {
        Vertex lastVertex = getLastVertex();
        Vertex vA = event.getEdge().getVertexA();
        if ((lastVertex != null) && (lastVertex != vA)) {
            throw new IllegalPathException("edge is not contiguous");
        }
    }

    public void afterEdgeAdded(GraphAddEdgeEvent event)
    {
        m_vertexList.add(event.getEdge().getVertexB());
    }

    public void beforeEdgeRemoved(GraphRemoveEdgeEvent event)
        throws Exception
    {
        if (event.getVertex() == null) {
            throw new IllegalPathException("edges cannot be removed");
        } else if (event.getVertex() != getLastVertex()) {
            throw new IllegalPathException("only last vertex can be removed");
        } else if (m_path.getDegree(getLastVertex()) > 1) {
            // REVIEW jvs 7-Mar-2001 -- See testRemoveCrossing for
            // an example of why this isn't really acceptable.
            throw new IllegalPathException(
                "crossing vertex cannot be removed");
        }
    }

    public GraphTraversal getTraversal()
    {
        return new PathTraversal(m_path);
    }

    public class PathTraversal extends GraphTraversal
    {
        PathTraversal(Path p_path) {
            super(p_path);
        }

        public int traverse(
            Vertex startat, List visited, Visitor visitor )
        {
            // find startat in m_vertexList
            int i = m_vertexList.indexOf(startat);
            if (i == -1) {
                throw new NoSuchElementException();
            }
            Iterator iter = m_vertexList.listIterator(i);
            while (iter.hasNext()) {
                Vertex v = (Vertex) iter.next();
                visited.add(v);
                if (!visitor.visit(v)) {
                    return TERMINATEDBYVISITOR;
                }
            }
            return OK;
        }

        public List traverse( Vertex startat )
        {
            return traverse(startat,new NullVisitor());
        }

        public List traverse( Vertex startat, Visitor visitor )
        {
            List visited = new ArrayList();
            traverse(startat, visited, visitor);
            return visited;
        }
    }

}

// End PathListener.java
