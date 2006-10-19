package salvo.jesus.graph.adaptor;
import salvo.jesus.graph.*;
import salvo.jesus.graph.algorithm.*;
import java.util.*;

/**
 * GraphDelegator is a utility base for creating adaptors which need to
 * delegate most of their methods to an underlying Graph.
 * 
 * @author John V. Sichi
 */
public abstract class GraphDelegator implements Graph
{
    private Graph m_graph;

    protected GraphDelegator(Graph graph)
    {
        m_graph = graph;
    }

    protected final Graph getGraph()
    {
        return m_graph;
    }

    public Set getVertexSet()
    {
        return m_graph.getVertexSet();
    }

    public Set getEdgeSet()
    {
        return m_graph.getEdgeSet();
    }

    public int getEdgesCount()
    {
        return m_graph.getEdgesCount();
    }
    
    public int getVerticesCount()
    {
        return m_graph.getVerticesCount();
    }
    
    public GraphFactory getGraphFactory()
    {
        return m_graph.getGraphFactory();
    }

    public void setGraphFactory(GraphFactory factory)
    {
        m_graph.setGraphFactory(factory);
    }

    public void add(Vertex v) throws Exception
    {
        m_graph.add(v);
    }

    public void remove(Vertex v) throws Exception
    {
        m_graph.remove(v);
    }

    public Iterator getVerticesIterator()
    {
        return m_graph.getVerticesIterator();
    }
    
    public List cloneVertices()
    {
        return m_graph.cloneVertices();
    }
    
    public Edge addEdge(Vertex v1,Vertex v2) throws Exception
    {
        return m_graph.addEdge(v1,v2);
    }
    
    public void addEdge(Edge e) throws Exception
    {
        m_graph.addEdge(e);
    }
    
    public void removeEdge(Edge e) throws Exception
    {
        m_graph.removeEdge(e);
    }

    public void removeEdges(Vertex v) throws Exception
    {
        m_graph.removeEdges(v);
    }
    
    public int getDegree()
    {
        return m_graph.getDegree();
    }
    
    public int getDegree(Vertex v)
    {
        return m_graph.getDegree(v);
    }

    public Set getVertices(int degree)
    {
        return m_graph.getVertices(degree);
    }

    public List getEdges(Vertex v)
    {
        return m_graph.getEdges(v);
    }
    
    public List getAdjacentVertices(Vertex v)
    {
        return m_graph.getAdjacentVertices(v);
    }

    public Set getAdjacentVertices(List vertices)
    {
        return m_graph.getAdjacentVertices(vertices);
    }

    public Collection getConnectedSet()
    {
        return m_graph.getConnectedSet();
    }
    
    public Set getConnectedSet(Vertex v)
    {
        return m_graph.getConnectedSet(v);
    }

    public List traverse(Vertex startat)
    {
        return m_graph.traverse(startat);
    }
    
    public GraphTraversal getTraversal()
    {
        return m_graph.getTraversal();
    }
    
    public void setTraversal(GraphTraversal traversal)
    {
        m_graph.setTraversal(traversal);
    }
    
    public boolean isConnected(Vertex v1,Vertex v2)
    {
        return m_graph.isConnected(v1,v2);
    }
    
    public void addListener(GraphListener listener)
    {
        m_graph.addListener(listener);
    }

    public void removeListener(GraphListener listener)
    {
        m_graph.removeListener(listener);
    }

    public void addGraphAddVertexListener(GraphAddVertexListener listener)
    {
        m_graph.addGraphAddVertexListener(listener);
    }

    public void addGraphAddEdgeListener(GraphAddEdgeListener listener)
    {
        m_graph.addGraphAddEdgeListener(listener);
    }
    
    public void addGraphRemoveEdgeListener(GraphRemoveEdgeListener listener)
    {
        m_graph.addGraphRemoveEdgeListener(listener);
    }

    public void addGraphRemoveVertexListener(
        GraphRemoveVertexListener listener)
    {
        m_graph.addGraphRemoveVertexListener(listener);
    }

    public void removeGraphAddVertexListener(GraphAddVertexListener listener)
    {
        m_graph.removeGraphAddVertexListener(listener);
    }
    
    public void removeGraphAddEdgeListener(GraphAddEdgeListener listener)
    {
        m_graph.removeGraphAddEdgeListener(listener);
    }

    public void removeGraphRemoveEdgeListener(
        GraphRemoveEdgeListener listener)
    {
        m_graph.removeGraphRemoveEdgeListener(listener);
    }
    
    public void removeGraphRemoveVertexListener(
        GraphRemoveVertexListener listener )
    {
        m_graph.removeGraphRemoveVertexListener(listener);
    }
}

// End GraphDelegator.java
