package salvo.jesus.graph;

import java.util.*;
import salvo.jesus.graph.algorithm.*;

/**
 * A directed Graph where edges have a specified direction. Edges in this
 * graph are therefore instances of DirectedEdge.
 *
 * @author		Jesus M. Salvo Jr.
 */
public class DirectedGraphImpl extends GraphImpl implements DirectedGraph {
  /**
   * Delegate object to handle the implementation of the DirectedGraph interface.
   */
  DirectedGraphWeakImpl   graphDirectionDelegate;

  /**
    * Creates a new instance of an empty directed Graph. The default
    * GraphTraversal object is an instance of DepthFirstDirectedGraphTraversal,
    * a depth-first traversal respecting the direction of edges.
    */
  public DirectedGraphImpl( ){
    super();
    this.factory = new DirectedGraphImplFactory();
    this.traversal = new DepthFirstDirectedGraphTraversal( this );
    this.graphDirectionDelegate = new DirectedGraphWeakImpl( this );
  }

  /**
    * Returns the outgoing edges of a particular Vertex in the Graph.
    *
    * @param		v		Vertex you want to determine its outgoing edges.
    * @return	  List of outgoing edges of the specified Vertex.
    */
  public List getOutgoingEdges( Vertex v ) {
    return this.graphDirectionDelegate.getOutgoingEdges( v );
  }

  /**
    * Returns the incoming edges of a particular Vertex in the Graph.
    *
    * @param		v		Vertex you want to determine its incoming edges.
    * @return	  List of incoming edges of the specified Vertex.
    */
  public List getIncomingEdges( Vertex v ) {
    return this.graphDirectionDelegate.getIncomingEdges( v );
  }

  /**
    * Returns the vertices that are adjacent to a specified Vertex
    * where the Edge is outgoing from the specified Vertex to the adjacent vertex.
    *
    * @param		v		Vertex you want to determine its outgoing adjacent vertices.
    * @return	  List of outgoing vertices adjacent to the specified Vertex.
    */
  public List getOutgoingAdjacentVertices( Vertex v ) {
    return this.graphDirectionDelegate.getOutgoingAdjacentVertices( v );
  }

  /**
    * Returns the vertices that are adjacent to a specified Vertex
    * where the Edge is incoming from the specified Vertex to the adjacent vertex.
    *
    * @param		v		Vertex you want to determine its incoming adjacent vertices.
    * @return	  List of incoming vertices adjacent to the specified Vertex.
    */
  public List getIncomingAdjacentVertices( Vertex v ) {
    return this.graphDirectionDelegate.getIncomingAdjacentVertices( v );
  }

  /**
    * Returns an Edge in the Graph whose origin is fromVertex and destination is toVertex.
    * If there is more than one Edge that has the same origin and destination in the Graph,
    * the first matching Edge is returned.
    *
    * @param		fromVertex		Vertex that is the origin of the directed Edge
    * @param		toVertex		Vertex that is the destination of the directed Edge
    * @return	Edge whose origin is fromVertex and destination is toVertex
    * @see			salvo.jesus.graph.Edge
    */
  public DirectedEdge getEdge( Vertex fromvertex, Vertex tovertex ) {
    return this.graphDirectionDelegate.getEdge( fromvertex, tovertex );
  }

  /**
   * Determines if there is a path from Vertex fromVertex to Vertex toVertex.
   * This will not return true if the only path has at least one Edge pointing
   * in the opposite direction of the path.
   *
   * @param		fromVertex		starting Vertex for the path
   * @param		toVertex			ending Vertex for the path
   * @return	true if there is a path from Vertex to toVertex. false otherwise.
   */
  public boolean isPath( Vertex fromVertex, Vertex toVertex ){
    return this.graphDirectionDelegate.isPath( fromVertex, toVertex );
  }

  /**
   * Determines if there is a cycle from Vertex fromVertex. A cycle occurs
   * when there is a path from the specified Vertex back to itself,
   * taking into consideration that direction of the Edges along the path.
   * This simply calls isPath(), where both parameters are the same Vertex.
   *
   * @param		fromVertex		Vertex to be tested for a cycle path.
   * @return	true if there is a cycle path from fromVertex to itself.
   */
  public boolean isCycle( Vertex fromVertex ){
    return this.graphDirectionDelegate.isCycle( fromVertex );
  }

    // REVIEW jvs 3-Feb-2002 -- is it worth trying to mimic the old toString
    // behavior?
}

