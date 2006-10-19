package salvo.jesus.graph;

import java.util.*;
import java.io.*;
import salvo.jesus.graph.algorithm.*;
import salvo.jesus.graph.adaptor.*;

/**
 * A weak implementation of the DirectedGraph interface.
 *
 * @author		Jesus M. Salvo Jr.
 */
class DirectedGraphWeakImpl extends GraphDelegator
    implements DirectedGraph, GraphListener
{
    /**
     * DirectedVertexData is the data structure kept for each vertex which is
     * part of this directed graph.  It mirrors GraphImpl.VertexData
     */
    private class DirectedVertexData implements Serializable
    {
        private ArrayList incomingEdges;
        private ArrayList outgoingEdges;
        private List unmodifiableIncomingEdges;
        private List unmodifiableOutgoingEdges;

        DirectedVertexData()
        {
            incomingEdges = new ArrayList();
            outgoingEdges = new ArrayList();
        }

        final List getIncomingEdges()
        {
            return incomingEdges;
        }

        final List getOutgoingEdges()
        {
            return outgoingEdges;
        }

        public final List getUnmodifiableIncomingEdges()
        {
            if (unmodifiableIncomingEdges == null) {
                unmodifiableIncomingEdges = Collections.unmodifiableList(
                    incomingEdges);
            }
            return unmodifiableIncomingEdges;
        }
        
        public final List getUnmodifiableOutgoingEdges()
        {
            if (unmodifiableOutgoingEdges == null) {
                unmodifiableOutgoingEdges = Collections.unmodifiableList(
                    outgoingEdges);
            }
            return unmodifiableOutgoingEdges;
        }
    }

    private Map vertexDataMap;
    
    /**
     * Creates a new instance of DirectedGraphWeakImpl.
     */
    public DirectedGraphWeakImpl( GraphImpl graph){
        super(graph);
        vertexDataMap = new HashMap();
        graph.addListener(this);
    }

    protected DirectedVertexData getVertexData(Vertex v)
    {
        return (DirectedVertexData) vertexDataMap.get(v);
    }

    /**
     * Returns the outgoing edges of a particular Vertex in the Graph.
     *
     * @param v Vertex you want to determine its outgoing edges.
     * @return List of outgoing edges of the specified Vertex.
     */
    public List getOutgoingEdges( Vertex v ) {
        return getVertexData(v).getUnmodifiableOutgoingEdges();
    }

    /**
     * Returns the incoming edges of a particular Vertex in the Graph.
     *
     * @param v Vertex you want to determine its incoming edges.
     * @return List of incoming edges of the specified Vertex.
     */
    public List getIncomingEdges( Vertex v ) {
        return getVertexData(v).getUnmodifiableIncomingEdges();
    }
    

    /**
     * Returns the vertices that are adjacent to a specified Vertex,
     * respecting the direction of the Edge from the specified Vertex.
     *
     * @param v Vertex you want to determine its outgoing adjacent vertices.
     * @param outGoing If true, method will return outgoing adjacent vertices.
     * If false, method will return incoming adjacent vertices.
     * @return List of outgoing / incoming vertices adjacent to the specified
     * Vertex.
     */
    private List getAdjacentVertices( Vertex v, boolean outGoing ) {
        List        adjacentVertices = new ArrayList( 10 );
        List        incidentEdges;
        Iterator    iterator;
        Edge        edge;
        Vertex      oppositeVertex;

        if( outGoing )
            incidentEdges = this.getOutgoingEdges( v );
        else
            incidentEdges = this.getIncomingEdges( v );

        iterator = incidentEdges.iterator();
        while( iterator.hasNext() ) {
            edge = (Edge) iterator.next();
            oppositeVertex = edge.getOppositeVertex( v );
            adjacentVertices.add( oppositeVertex );
        }

        return Collections.unmodifiableList(adjacentVertices);
    }

    /**
     * Returns the vertices that are adjacent to a specified Vertex where the
     * Edge is outgoing from the specified Vertex to the adjacent vertex.
     *
     * @param v Vertex you want to determine its outgoing adjacent vertices.
     * @return List of outgoing vertices adjacent to the specified Vertex.
     */
    public List getOutgoingAdjacentVertices( Vertex v ) {
        return this.getAdjacentVertices( v, true );
    }

    /**
     * Returns the vertices that are adjacent to a specified Vertex where the
     * Edge is incoming from the specified Vertex to the adjacent vertex.
     *
     * @param v Vertex you want to determine its incoming adjacent vertices.
     * @return List of incoming vertices adjacent to the specified Vertex.
     */
    public List getIncomingAdjacentVertices( Vertex v ) {
        return this.getAdjacentVertices( v, false );
    }

    /**
     * Returns an Edge in the Graph whose origin is fromVertex and destination
     * is toVertex.  If there is more than one Edge that has the same origin
     * and destination in the Graph, the first matching Edge is returned.
     *
     * @param fromVertex Vertex that is the origin of the directed Edge
     * @param toVertex Vertex that is the destination of the directed Edge
     * @return Edge whose origin is fromVertex and destination is toVertex
     * @see salvo.jesus.graph.Edge
     */
    public DirectedEdge getEdge( Vertex fromvertex, Vertex tovertex ) {
        List        outIncidentEdges;
        Iterator	iterator;
        DirectedEdge		edge;

        // Get the adjacent edge set of the from vertex
        outIncidentEdges = this.getOutgoingEdges( fromvertex );

        // Find the edge where the direction is to the tovertex
        iterator = outIncidentEdges.iterator();
        while( iterator.hasNext()) {
            edge = (DirectedEdge) iterator.next();
            if( edge.getSink() == tovertex ) {
                // Edge is found.
                iterator = null;
                return edge;
            }
        }
        return null;
    }

    /**
     * Determines if there is a path from Vertex fromVertex to Vertex toVertex.
     * This will not return true if the only path has at least one Edge
     * pointing in the opposite direction of the path.
     *
     * @param fromVertex starting Vertex for the path
     * @param toVertex ending Vertex for the path
     * @return true if there is a path from Vertex to toVertex. false
     * otherwise.
     */
    public boolean isPath( Vertex fromVertex, Vertex toVertex ){
        StopAtVisitor visitor = new StopAtVisitor(toVertex);
        getGraph().getTraversal().traverse(fromVertex, visitor);
        return visitor.wasFound();
    }

    /**
     * Determines if there is a cycle from Vertex fromVertex. A cycle occurs
     * when there is a path from the specified Vertex back to itself, taking
     * into consideration that direction of the Edges along the path.
     *
     * @param fromVertex Vertex to be tested for a cycle path.
     * @return true if there is a cycle path from fromVertex to itself.
     */
    public boolean isCycle( Vertex fromVertex ){
        List            outedges = this.getOutgoingEdges( fromVertex );
        Iterator        iterator = outedges.iterator();
        DirectedEdge    dedge;
        Vertex          adjacentVertex;

        // REVIEW jvs 2-Feb-2002 -- seems like we should be able to
        // tweak the DFS implementation to allow us to skip fromVertex
        // initially so we wouldn't have to iterate over the edges here
        
        // For each outgoing edge of the vertex ...
        while( iterator.hasNext() ){
            dedge = (DirectedEdge) iterator.next();
            // ... get the opposite vertex
            adjacentVertex = dedge.getOppositeVertex( fromVertex );
            // .. and check if there is a path from the opposite vertex back to
            // the vertex
            if( this.isPath( adjacentVertex, fromVertex ))
                // There is a cycle
                return true;
        }

        // No cycle
        return false;
    }

    // -- GraphListener implementation --
    
    // we don't care about most before vertex events
    public void beforeVertexAdded(GraphAddVertexEvent event)
    {
    }
    
    public void beforeVertexRemoved(GraphRemoveVertexEvent event)
    {
    }
    
    // but we do care about this before event; use this as a chance to make
    // sure that the Edge being added is a DirectedEdge 
    public void beforeEdgeAdded(GraphAddEdgeEvent event)
        throws ClassCastException
    {
        DirectedEdge dedge = (DirectedEdge) event.getEdge();
    }
    
    public void beforeEdgeRemoved(GraphRemoveEdgeEvent event)
    {
    }

    // and we do care about after events; this is where we keep the
    // directed graph data structures in sync with the undirected
    
    public void afterVertexAdded(GraphAddVertexEvent event)
    {
        vertexDataMap.put(event.getVertex(),new DirectedVertexData());
    }
    
    public void afterVertexRemoved(GraphRemoveVertexEvent event)
    {
        vertexDataMap.remove(event.getVertex());
    }
    
    public void afterEdgeAdded(GraphAddEdgeEvent event)
    {
        DirectedEdge dedge = (DirectedEdge) event.getEdge();
        
        DirectedVertexData vd1 = getVertexData(dedge.getSource());
        DirectedVertexData vd2 = getVertexData(dedge.getSink());
        vd1.getOutgoingEdges().add(dedge);
        vd2.getIncomingEdges().add(dedge);
    }

    public void afterEdgeRemoved(GraphRemoveEdgeEvent event)
    {
        Vertex	fromvertex;
        Vertex	tovertex;
        List    outIncidentEdges;
        List    inIncidentEdges;

        DirectedEdge dedge = (DirectedEdge) event.getEdge();
        
        // Get source and sink vertices of edge
        fromvertex = dedge.getSource();
        tovertex = dedge.getSink();

        // Get the vector of outgoing edge of the source and the
        // vector of incoming edges of the sink.
        DirectedVertexData vdFrom = getVertexData(fromvertex);
        DirectedVertexData vdTo = getVertexData(tovertex);

        // Remove the edge from the source's outgoing edges
        vdFrom.getOutgoingEdges().remove( dedge );
        // Remove the edge from the sink's incoming edges
        vdTo.getIncomingEdges().remove( dedge );
    }
    
}
