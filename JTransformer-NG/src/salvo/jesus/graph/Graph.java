package salvo.jesus.graph;

import java.util.*;
import java.io.*;
import salvo.jesus.graph.algorithm.*;

/**
 * An interface for Graphs.
 *
 *<p>
 *
 * Note that all methods which return collections return unmodifiable
 * collections; callers that need modifiable collections must
 * copy the unmodifiable collections explicitly using collection constructors
 * (e.g. new ArrayList(graph.getEdgeSet())).  The reason is that
 * the returned collections may be owned by the graph and so should
 * not be modified by callers.
 *
 * $Id$
 */
public interface Graph extends Serializable {

  // ------------------- Informational methods
  /**
    * Returns the number of vertices in the graph
    *
    * @return	The number of vertices in the graph.
    */
  public int getVerticesCount();

  // ------------------- Factory setup
  /**
   * Returns the factory that will be responsible for creating Vertices
   * and Edges in a Graph.
   */
  public GraphFactory getGraphFactory();

  /**
   * Sets the factory that will be responsible for creating Vertices
   * and Edges in a Graph.
   */
  public void setGraphFactory( GraphFactory factory );

  // ------------------- Vertex manipulation
  /**
    * Adds a Vertex into the Graph.  All GraphListeners are informed
    * of the event that a Vertex has been added to the Graph.
    * Note that this method is not called when a vertex is added
    * implicitly due to edge addition.
    *
    * @param		v		Vertex to be added to the Graph
    */
  public void add( Vertex v ) throws Exception;

  /**
   * Removes the specified Vertex and all of its incident edges from the
   * Graph.  All GraphListeners are informed of events for the vertex
   * and edges.
   *
   * @param   edge    The Edge object to be removed.
   */
  public void remove( Vertex v ) throws Exception;

  /**
   * Returns an iterator that iterates through the graph's vertices in
   * arbitrary order.
   *
   * @return  An iterator of List vertices.
   */
  public Iterator getVerticesIterator();

  /**
   * Returns a modifiable clone of the vertex set as a List in
   * arbitrary order.
   *
   * @return  List of Vertices
   */
  public List cloneVertices();

  /**
   * Returns an unmodifiable set of the vertices included in this graph.
   *
   * @return Set of Vertices
   */
  public Set getVertexSet();

  // ----------------------- Edge manipulation
  /**
    * Creates an edge between two vertices and adds it into the Graph.
    * All GraphListeners are informed of the event that a
    * Edge has been added to the Graph.  If the vertices of the Edge are
    * not already part of the graph, they are implicitly added, and listeners
    * are notified of them as well.
    *
    * @param		v1	One endpoint of the edge
    * @param    v2  Other endpoint of the edge
    * @return   The Edge object created and added to the Graph.
    */
  public Edge addEdge( Vertex v1, Vertex v2 ) throws Exception;


  /**
    * Adds a previously-defined Edge into the Graph.  All GraphListeners
    * are informed of the event that the Edge
    * has been added to the Graph.
    * If the vertices of the Edge are not already
    * part of the graph, they are implicitly added, and listeners
    * are notified of them as well.
    *
    *<p>
    *
    *<b>Note:</b> It is the caller's responsibility to make sure
    * that the type of Edge being added to the Graph matches the Graph. For
    * example, only a DirectedEdge must be added to a DirectedGraph.
    *
    * @param	e   The edge to be added to the Graph.
    */
  public void addEdge( Edge e ) throws Exception;

  /**
   * Removes the specified Edge from the Graph, and notifies listeners.
   * Note that this method is not called when an edge is removed
   * implicitly due to vertex removal.
   *
   * @param   e    The Edge object to be removed.
   */
  public void removeEdge( Edge e ) throws Exception;

  /**
    * Removes incident Edges of a Vertex. The Edges removed are those whose
    * either endpoints has the specified vertex. This method leaves
    * the specified Vertex as a disconnected singleton.  Note that this method
    * is not called implicitly when a vertex is removed.
    *
    * @param		v	Vertex whose Edges are to be removed
    */
  public void removeEdges( Vertex v ) throws Exception;

  /**
   * Returns an unmodifiable set of the edges included in this graph.
   *
   * @return Set of Edges
   */
  public Set getEdgeSet();

  /**
   * Returns the number of edges in the graph
   *
   * @return	The number of edges in the graph.
   */
  public int getEdgesCount();

  // --------------------------- Degree methods
  /**
   * Returns the degree of the graph, which is simply the highest degree
   * of all the graph's vertices.
   *
   * @return  An int indicating the degree of the graph.
   */
  public int getDegree();

  /**
   * Returns the degree of the vertex, which is simply the number of edges
   * of the vertex.
   *
   * @return  The degree of the vertex.
   */
  public int getDegree( Vertex v );

  /**
   * Returns all vertices with the specified degree.
   *
   * @param   degree    The degree of the vertex to be returned.
   * @return  an unmodifiable set of vertices with the above specified degree.
   */
  public Set getVertices( int degree );

  // ---------------------- Adjacency methods
  /**
   * Returns a List of edges of the specified vertex.
   *
   * @param   v   The vertex whose edges we want returned
   * @return an unmodifiable List of Edges that are incident edges of the
   * specified vertex.
   */
  public List getEdges( Vertex v );

  /**
    * Returns the vertices adjacent to the specified vertex.
    *
    * @param		v		The Vertex you want to determine its adjacent vertices.
    * @return an unmodifiable List of vertices adjacent to the specified vertex
    * v.
    */
  public List getAdjacentVertices( Vertex v );

  /**
    * Returns the vertices adjacent to all the vertices in the given collection.
    *
    * @param		vertices		List of Vertex where each vertex in the returned Set
    *                               must be adjacent to.
    * @return unmodifiable Set of vertices adjacent to all the vertices in the
    * supplied List.
    */
  public Set getAdjacentVertices( List vertices );

  // ------------------------ Connected set methods
  /**
    * Returns the connected sets in the Graph. Each Set in the return
    * Collection is a Set of vertices that are connected to each other,
    * regardless of the direction or length of the paths connecting them
    * together.
    *
    * @return unmodifiable Collection of unmodifiable Sets
    */
  public Collection getConnectedSet();

  /**
    * Returns the connected set to which the specified vertex belongs.
    *
    * @param		v		Vertex to which you want the connected set returned.
    * @return unmodifiable Set of connected vertices where the specified vertex
    * belongs.
    */
  public Set getConnectedSet( Vertex v );

  // ------------------------ Traversal methods
  /**
   *  Traverses the Graph starting at startat Vertex by performing a
   *  depth-first traversal. The vertices traversed from startat
   *  are stored in Visited. Only the connected
   *  components to which startat belongs to will be traversed.
   *
   *	@param	startat	 The Vertex to which you want to start the traversal.
   */
  public List traverse( Vertex startat );

  /**
   * Gets the traversal algorithm used by the Graph.
   *
   * @return  GraphTraversal object performing traversal for the Graph.
   */
  public GraphTraversal getTraversal();

  /**
   * Sets the graph traversal algorithm to be used
   *
   * @param traversal   A concrete implementation of the GraphTraversal object.
   */
  public void setTraversal( GraphTraversal traversal );

  // ------------------------ connectivity methods
  /**
   * Determines if two vertices are connected
   *
   * @param		v1		  starting Vertex for the path
   * @param		v2			ending Vertex for the path
   * @return	true if v1 and v2 are connected.
   */
  public boolean isConnected( Vertex v1, Vertex v2 );

  // ------------------------ Listener methods

  /**
   * Registers a GraphListener with this Graph.
   *
   * @param listener the listener being registered, which will receive
   * notifications of all graph modifications
   */
  public void addListener(GraphListener listener);

  /**
   * Unregisters a GraphListener currently attached to this Graph.
   *
   * @param listener the listener being unregistered, which will
   * cease receiving notifications
   */
  public void removeListener(GraphListener listener);

  /**
    * Adds a GraphAddVertexListener to the Graph's internal List of
    * GraphAddVertexListeners so that when a new Vertex is added,
    * all registered GraphAddVertedListeners are notified of the event.
    *
    * @param		listener		GraphAddVertexListener you want registered
    * or be notified when a new Vertex is added
    * @see			salvo.jesus.graph.GraphAddVertexListener
    * @see			#removeGraphAddVertexListener( GraphAddVertexListener )
    * @deprecated Use addListener instead
    * @see #addListener
    */
  public void addGraphAddVertexListener( GraphAddVertexListener listener );

  /**
    * Adds a GraphAddEdgeListener to the Graph's internal List of
    * GraphAddEdgeListeners so that when a new Edge is added,
    * all registered GraphAddEdgeListeners are notified of the event.
    *
    * @param		listener		GraphAddEdgeListener you want registered
    * or be notified when a new Edge is added
    * @see			salvo.jesus.graph.GraphAddEdgeListener
    * @see			#removeGraphAddEdgeListener( GraphAddEdgeListener )
    * @deprecated Use addListener instead
    * @see #addListener
    */
  public void addGraphAddEdgeListener( GraphAddEdgeListener listener );

  /**
    * Adds a GraphRemoveEdgeListener to the Graph's internal List of
    * GraphRemoveEdgeListeners so that when an Edge is removed,
    * all registered GraphRemoveEdgeListeners are notified of the event.
    *
    * @param		listener		GraphRemoveEdgeListener you want registered
    * or be notified when an Edge is removed
    * @see			salvo.jesus.graph.GraphRemoveEdgeListener
    * @see			#removeGraphRemoveEdgeListener( GraphRemoveEdgeListener )
    * @deprecated Use addListener instead
    * @see #addListener
    */
  public void addGraphRemoveEdgeListener( GraphRemoveEdgeListener listener );

  /**
    * Adds a GraphRemoveVertexListener to the Graph's internal List of
    * GraphRemoveVertexListeners so that when a Vertex is removed,
    * all registered GraphRemoveVertexListeners are notified of the event.
    *
    * @param		listener		GraphRemoveVertexListener you want registered
    * or be notified when a Vertex is removed
    * @see			salvo.jesus.graph.GraphRemoveVertexListener
    * @see			#removeGraphRemoveVertexListener( GraphRemoveVertexListener )
    * @deprecated Use addListener instead
    * @see #addListener
    */
  public void addGraphRemoveVertexListener( GraphRemoveVertexListener listener );

  /**
    * Removes a GraphAddVertexListener from the Graph's internal List of
    * GraphAddVertexListeners.
    *
    * @param		listener		GraphAddVertexListener you no longer want registered
    * or be notified when a Vertex is added
    * @see			salvo.jesus.graph.GraphAddVertexListener
    * @see			#addGraphAddVertexListener( GraphAddVertexListener )
    * @deprecated Use removeListener instead
    * @see #removeListener
    */
  public void removeGraphAddVertexListener( GraphAddVertexListener listener );

  /**
    * Removes a GraphAddEdgeListener from the Graph's internal List of
    * GraphAddEdgeListeners.
    *
    * @param		listener		GraphAddEdgeListener you no longer want registered
    * or be notified when an Edge is added
    * @see			salvo.jesus.graph.GraphAddEdgeListener
    * @see			#addGraphAddEdgeListener( GraphAddEdgeListener )
    * @deprecated Use removeListener instead
    * @see #removeListener
    */
  public void removeGraphAddEdgeListener( GraphAddEdgeListener listener );

  /**
    * Removes a GraphRemoveEdgeListener from the Graph's internal List of
    * GraphRemoveEdgeListeners.
    *
    * @param		listener		GraphRemoveEdgeListener you no longer want registered
    * or be notified when an Edge is removed
    * @see			salvo.jesus.graph.GraphRemoveEdgeListener
    * @see			#addGraphRemoveEdgeListener( GraphRemoveEdgeListener )
    * @deprecated Use removeListener instead
    * @see #removeListener
    */
  public void removeGraphRemoveEdgeListener( GraphRemoveEdgeListener listener );

  /**
    * Removes a GraphRemoveVertexListener from the Graph's internal List of
    * GraphRemoveVertexListeners.
    *
    * @param		listener		GraphRemoveVertexListener you no longer want registered
    * or be notified when a Vertex is removed
    * @see			salvo.jesus.graph.GraphRemoveVertexListener
    * @see			#addGraphRemoveVertexListener( GraphRemoveVertexListener )
    * @deprecated Use removeListener instead
    * @see #removeListener
    */
  public void removeGraphRemoveVertexListener( GraphRemoveVertexListener listener );

}

