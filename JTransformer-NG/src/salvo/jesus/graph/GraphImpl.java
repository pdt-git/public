package salvo.jesus.graph;

import java.util.*;
import java.io.*;
import salvo.jesus.graph.algorithm.*;
import salvo.jesus.graph.listener.*;

/**
 * An implementation of the Graph interface. GraphImpl
 * represents a materialized graph data structure in the form of a
 * set of incidence lists (one for each vertex).
 * GraphImpl relies on hashing, so the Vertex and Edge implementations
 * used must have well-behaved implementations of equals and hashCode (the
 * basic Object implementations of these methods work and are the most
 * efficient for most uses).
 *
 * $Id$
 */
public class GraphImpl implements Graph {

    /**
     * VertexData is the data structure kept for each vertex which is part of
     * this graph.
     */
    private static class VertexData implements Serializable
    {
        /**
         * The incidence list for each vertex is kept in a dynamic array.
         */
        private List incidentEdges;

        /**
         * unmodifiableIncidentEdges is an unmodifiable wrapper for
         * getIncidentEdges(), returned to callers whom we don't trust because
         * they might accidentally modify the incident edge list
         */
        private List unmodifiableIncidentEdges;

        VertexData()
        {
            incidentEdges = new ArrayList();
        }

        /**
         * The incident edge list for this vertex, defined by subclass
         * implementations.
         */
        final List getIncidentEdges()
        {
            return incidentEdges;
        }

        /**
         * Public accessor provides read-only access.
         */
        final List getUnmodifiableIncidentEdges()
        {
            if (unmodifiableIncidentEdges == null) {
                unmodifiableIncidentEdges = Collections.unmodifiableList(
                    getIncidentEdges());
            }
            return unmodifiableIncidentEdges;
        }
    }

    /**
     * Reference to the instance of <tt>GraphFactory</tt>
     * responsible for creating Vertices and Edges.
     */
    protected GraphFactory factory;

    /**
     * Set of adjacency lists for all vertices in the graph.  Each entry in the
     * map has a Vertex as key and an instance of VertexData as value.
     */
    private Map vertexDataMap;

    /**
     * An unmodifiable wrapper for vertexDataMap.keySet(), returned to callers
     * whom we don't trust because they might accidentally modify vertexDataMap.
     */
    private transient Set unmodifiableVertexSet;

    /**
     * Set of edges in this graph.  This is redundant with vertexDataMap, but
     * is maintained for two purposes:  (1) to allow a constant-time
     * duplicate-edge test; (2) to provide convenient iteration over all edges
     * in the graph.
     */
    private Set edgeSet;

    /**
     * An unmodifiable wrapper for edgeSet, returned to callers whom we don't
     * trust because they might accidentally modify edgeSet.
     */
    private transient Set unmodifiableEdgeSet;

    /**
     * List<GraphListener> with all listeners interested in receiving
     * notifications about modifications to this Graph.
     */
    private List listenerList;

    /**
     * A listener for incrementally maintaining connected set information
     * about this graph.  If this is non-null, then it is also registered
     * in listenerList.
     */
    private ConnectedSetListener connectedSetListener;

    /**
     * Delegate object for implementing graph traversal. The default
     * implementation is DepthFirstGraphTraversal.
     */
    protected  GraphTraversal    traversal;

    public GraphImpl() {
        vertexDataMap = new HashMap();
        edgeSet = new HashSet();
        listenerList = new ArrayList(10);

        this.factory = new GraphImplFactory();

        traversal = new DepthFirstGraphTraversal( this );
    }

    /**
     * Returns the factory that will be responsible for creating Vertices
     * and Edges in a Graph.
     */
    public GraphFactory getGraphFactory() {
        return this.factory;
    }

    /**
     * Sets the factory that will be responsible for creating Vertices
     * and Edges in a Graph.
     */
    public void setGraphFactory( GraphFactory factory ) {
        this.factory = factory;
    }

    /**
     * Returns a read-only iterator that iterates through the graph's vertices.
     *
     * @return  An iterator of vertices.
     */
    public Iterator getVerticesIterator() {
        return getVertexSet().iterator();
    }

    /**
     * Returns a clone of the List of vertices.
     *
     * @return  A clone of the List of vertices.
     */
    public List cloneVertices() {
        return new ArrayList(getVertexSet());
    }

    /**
     * @see Graph#getVertexSet
     */
    public Set getVertexSet()
    {
        // NOTE:  this and getEdgeSet() are computed on demand because
        // they have to be transient fields for Serializable

        if (unmodifiableVertexSet == null) {
            unmodifiableVertexSet = Collections.unmodifiableSet(
                vertexDataMap.keySet());
        }
        return unmodifiableVertexSet;
    }

    /**
     * @see Graph#getEdgeSet
     */
    public Set getEdgeSet()
    {
        if (unmodifiableEdgeSet == null) {
            unmodifiableEdgeSet = Collections.unmodifiableSet(edgeSet);
        }
        return unmodifiableEdgeSet;
    }

    /**
     * Test whether a vertex is included in this graph.
     *
     * @param v the vertex to test
     *
     * @return true iff the vertex is included
     */
    public boolean containsVertex(Vertex v)
    {
        return this.vertexDataMap.containsKey(v);
    }

    /**
     * Test whether an edge is included in this graph.
     *
     * @param edge the edge to test
     *
     * @return true iff the edge is included
     */
    public boolean containsEdge(Edge edge)
    {
        return edgeSet.contains(edge);
    }

    /**
     * Returns an unmodifiable List of edges of the specified vertex.
     *
     * @param   v   The vertex whose edges we want returned
     * @return A List of Edges that are incident edges of the specified vertex.
     */
    public List getEdges( Vertex v ) {
        return getVertexData(v).getUnmodifiableIncidentEdges();
    }

    private VertexData getVertexData(Vertex v)
    {
        return (VertexData) vertexDataMap.get(v);
    }

    private List getIncidentEdges(Vertex v)
    {
        return getVertexData(v).getIncidentEdges();
    }

    private void invokeAddVertexListeners(
        GraphAddVertexEvent event,boolean before)
        throws Exception
    {
        Iterator iter = listenerList.iterator();
        while (iter.hasNext()) {
            GraphListener graphListener = (GraphListener) iter.next();
            if (before) {
                graphListener.beforeVertexAdded(event);
            } else {
                graphListener.afterVertexAdded(event);
            }
        }
    }

    private void invokeRemoveVertexListeners(
        GraphRemoveVertexEvent event,boolean before)
        throws Exception
    {
        Iterator iter = listenerList.iterator();
        while (iter.hasNext()) {
            GraphListener graphListener = (GraphListener) iter.next();
            if (before) {
                graphListener.beforeVertexRemoved(event);
            } else {
                graphListener.afterVertexRemoved(event);
            }
        }
    }

    private void invokeAddEdgeListeners(
        GraphAddEdgeEvent event,boolean before)
        throws Exception
    {
        Iterator iter = listenerList.iterator();
        while (iter.hasNext()) {
            GraphListener graphListener = (GraphListener) iter.next();
            if (before) {
                graphListener.beforeEdgeAdded(event);
            } else {
                graphListener.afterEdgeAdded(event);
            }
        }
    }

    private void invokeRemoveEdgeListeners(
        GraphRemoveEdgeEvent event,boolean before)
        throws Exception
    {
        Iterator iter = listenerList.iterator();
        while (iter.hasNext()) {
            GraphListener graphListener = (GraphListener) iter.next();
            if (before) {
                graphListener.beforeEdgeRemoved(event);
            } else {
                graphListener.afterEdgeRemoved(event);
            }
        }
    }

    /**
     * This implementation of add(Vertex) should not normally be
     * overridden by subclasses.  Instead, subclasses should
     * set up listeners to receive notifications of vertex additions.
     *
     * @see Graph#add
     */
    public void add( Vertex newvertex ) throws Exception {
        GraphAddVertexListener	listener;

        if (this.containsVertex(newvertex)) {
            // vertex is already part of this graph; nothing to do
            return;
        }

        GraphAddVertexEvent event =
            new GraphAddVertexEvent(this,newvertex,null);
        invokeAddVertexListeners(event,true);
        addVertexUnconditionally(event);
    }

    /**
     * Do the real work of adding a vertex, including invoking
     * after-listeners.  Before-listeners are invoked outside of this method,
     * since they may abort the vertex-adding process.
     */
    private void addVertexUnconditionally(GraphAddVertexEvent event)
        throws Exception
    {
        vertexDataMap.put(event.getVertex(),new VertexData());
        invokeAddVertexListeners(event,false);
    }

    /**
     * This implementation of addEdge should not normally be
     * overridden by subclasses.  Instead, subclasses should
     * set up listeners to receive notifications of edge additions.
     *
     * @see Graph#addEdge(Vertex,Vertex)
     */
    public Edge addEdge( Vertex v1, Vertex v2 ) throws Exception {
        Edge edge = this.factory.createEdge( v1, v2 );
        addEdge( edge );
        return edge;
    }

    /**
     * This implementation of addEdge should not normally be
     * overridden by subclasses.  Instead, subclasses should
     * set up listeners to receive notifications of edge additions.
     *
     * @see Graph#addEdge(Edge)
     */
    public void addEdge( Edge edge ) throws Exception {
        if (this.containsEdge(edge)) {
            // edge is already part of this graph; nothing to do
            return;
        }

        Vertex v1 = edge.getVertexA();
        Vertex v2 = edge.getVertexB();
        GraphAddVertexEvent v1addEvent = null;
        GraphAddVertexEvent v2addEvent = null;

        if (!this.containsVertex(v1)) {
            v1addEvent = new GraphAddVertexEvent(this,v1,edge);
        }
        if ((v2 != v1) && !this.containsVertex(v2)) {
            v2addEvent = new GraphAddVertexEvent(this,v2,edge);
        }

        // invoke relevant before-listeners
        if (v1addEvent != null) {
            invokeAddVertexListeners(v1addEvent,true);
        }
        if (v2addEvent != null) {
            invokeAddVertexListeners(v2addEvent,true);
        }

        GraphAddEdgeEvent event = new GraphAddEdgeEvent(
            this,
            edge,
            v1addEvent != null,
            v2addEvent != null);
        invokeAddEdgeListeners(event,true);

        // we have the go-ahead from all before-listeners; now do the real work

        if (v1addEvent != null) {
            addVertexUnconditionally(v1addEvent);
        }
        if (v2addEvent != null) {
            addVertexUnconditionally(v2addEvent);
        }

        List v1edges = this.getIncidentEdges( v1 );

        // Add the edge as an incident edge of both vertices
        v1edges.add( edge );
        if (v1 != v2) {
            // don't add the edge twice in the case of a self-loop
            List v2edges = this.getIncidentEdges( v2 );
            v2edges.add( edge );
        }

        // also add the edge to edgeSet
        edgeSet.add(edge);

        invokeAddEdgeListeners(event,false);
    }

    /**
     * This implementation of remove should not normally be
     * overridden by subclasses.  Instead, subclasses should
     * set up listeners to receive notifications of vertex removals.
     *
     * @see Graph#remove
     */
    public void remove( Vertex v ) throws Exception {
        beforeRemoveEdges(v,true);

        GraphRemoveVertexEvent event = new GraphRemoveVertexEvent(this,v);
        invokeRemoveVertexListeners(event,true);

        // we have the go-ahead from all before-listeners; now do the real work

        removeEdgesUnconditionally(v,true);

        vertexDataMap.remove(v);
        invokeRemoveVertexListeners(event,false);
    }

    /**
     * This implementation of removeEdge should not normally be
     * overridden by subclasses.  Instead, subclasses should
     * set up listeners to receive notifications of edge removals.
     *
     * @see Graph#removeEdge
     */
    public void removeEdge( Edge edge ) throws Exception {
        GraphRemoveEdgeEvent event = new GraphRemoveEdgeEvent(this,edge,null);
        invokeRemoveEdgeListeners(event,true);
        removeEdgeUnconditionally(event);
    }

    /**
     * Complete the work of removeEdge, after all
     * before-listeners have been notified.
     */
    private void removeEdgeUnconditionally(GraphRemoveEdgeEvent event)
        throws Exception
    {
        // Remove the edge from the vertices incident edges.
        Edge edge = event.getEdge();
        Vertex v1 = edge.getVertexA();
        List v1edges = this.getIncidentEdges( v1 );
        v1edges.remove( edge );

        Vertex v2 = edge.getVertexB();
        List v2edges = this.getIncidentEdges( v2 );
        v2edges.remove( edge );

        // Remove the edge from edgeSet
        edgeSet.remove(edge);

        invokeRemoveEdgeListeners(event,false);
    }

    /**
     * This implementation of removeEdges should not normally be
     * overridden by subclasses.  Instead, subclasses should
     * set up listeners to receive notifications of edge removals.
     *
     * @see Graph#removeEdges
     */
    public void removeEdges( Vertex v ) throws Exception
    {
        beforeRemoveEdges(v,false);
        removeEdgesUnconditionally(v,false);
    }

    /**
     * Invoke before-listeners for removal of all edges incident
     * to v.
     */
    private void beforeRemoveEdges(Vertex v,boolean willRemoveVertex)
        throws Exception
    {
        Iterator iterator = getIncidentEdges(v).iterator();
        while( iterator.hasNext( )) {
            Edge edgetoremove = (Edge) iterator.next();
            GraphRemoveEdgeEvent event = new GraphRemoveEdgeEvent(
                this,edgetoremove,willRemoveVertex ? v : null);
            invokeRemoveEdgeListeners(event,true);
        }
    }

    /**
     * Complete the work of removeEdges, after all
     * before-listeners have been notified.
     */
    private void removeEdgesUnconditionally(Vertex v,boolean willRemoveVertex)
        throws Exception
    {
        // Remove incident edges of vertex; clone edge list
        // to avoid iteration/modification conflict.
        List vedges = new ArrayList(this.getIncidentEdges( v ));
        Iterator iterator = vedges.iterator();
        while( iterator.hasNext( )) {
            Edge edgetoremove = (Edge) iterator.next();
            GraphRemoveEdgeEvent event = new GraphRemoveEdgeEvent(
                this,edgetoremove,willRemoveVertex ? v : null);
            this.removeEdgeUnconditionally( event );
        }
    }

    /**
     * Returns the number of vertices in the graph
     *
     * @return	The number of vertices in the graph.
     */
    public int getVerticesCount() {
        return vertexDataMap.size();
    }

    /**
     * @see Graph#getEdgesCount
     */
    public int getEdgesCount() {
        return edgeSet.size();
    }

    /**
     * Returns all vertices with the specified degree.
     *
     * @param   degree    The degree of the vertex to be returned.
     * @return  A collection of vertices with the above specified degree.
     */
    public Set getVertices( int degree ) {
        Set       verticesofsamedegree = new HashSet();
        Iterator  iterator;
        Vertex    vertex;

        iterator = this.getVerticesIterator();
        while( iterator.hasNext() ) {
            vertex = (Vertex) iterator.next();
            if( this.getAdjacentVertices( vertex ).size() == degree )
                verticesofsamedegree.add( vertex );
        }

        return Collections.unmodifiableSet(verticesofsamedegree);
    }

    /**
     * Returns the vertices adjacent to the specified vertex.
     *
     * @param v The Vertex you want to determine its adjacent vertices.
     * @return List of vertices adjacent to the specified vertex v.
     */
    public List getAdjacentVertices( Vertex v ) {
        List    adjacentVertices = new ArrayList( 10 );
        List      incidentEdges = this.getEdges( v );
        Iterator  iterator;
        Edge      edge;
        Vertex    oppositeVertex;

        if( incidentEdges != null ) {
            iterator = incidentEdges.iterator();
            while( iterator.hasNext() ) {
                edge = (Edge) iterator.next();
                oppositeVertex = edge.getOppositeVertex( v );
                if( oppositeVertex != null )
                    adjacentVertices.add( oppositeVertex );
            }
        }

        return Collections.unmodifiableList(adjacentVertices);
    }

    /**
     * Returns the vertices adjacent to all the vertices in the given
     * collection.
     *
     * @param vertices List of Vertex where each vertex in the returned Set
     * must be adjacent to.
     * @return Set of vertices adjacent to all the vertices in the supplied
     * List.
     */
    public Set getAdjacentVertices( List vertices ) {
        HashSet adjacentVertices = new HashSet(
            this.getAdjacentVertices( (Vertex) vertices.get(0)) );
        int i, size = vertices.size();

        for( i = 1; i < size; i++ ) {
            adjacentVertices.retainAll(
                this.getAdjacentVertices( (Vertex) vertices.get(i)));
        }

        return Collections.unmodifiableSet(adjacentVertices);
    }

    /**
     * Construct a ConnectedSetListener for this graph if there isn't
     * one already.
     *
     * @return the new or existing ConnectedSetListener
     */
    private ConnectedSetListener getConnectedSetListener()
    {
        if (connectedSetListener == null) {
            connectedSetListener = new ConnectedSetListener(this);
            addListener(connectedSetListener);
        }
        return connectedSetListener;
    }

    /**
     * @see Graph#getConnectedSet()
     */
    public Collection getConnectedSet( ){
        return getConnectedSetListener().getConnectedSets();
    }

    /**
     * @see Graph#getConnectedSet(Vertex)
     */
    public Set getConnectedSet( Vertex v ){
        return getConnectedSetListener().getConnectedSet(v);
    }

    /**
     * Forget any information which has been incrementally maintained
     * about this graph's connected sets.  Such information is computed
     * on demand, so before doing a large number of graph updates, it's a good
     * idea to call this method if one of the getConnectedSet methods has been
     * previously called.
     */
    public void forgetConnectedSets()
    {
        if (connectedSetListener == null) {
            // can't forget something you don't remember
            return;
        }
        removeListener(connectedSetListener);
        connectedSetListener = null;
    }

    /**
     *  Traverses the Graph starting at startat Vertex.  Only the connected
     *  components to which startat belongs to will be traversed.
     */
    public List traverse( Vertex startat ){
        return traversal.traverse( startat );
    }

    /**
     * Gets the traversal algorithm used by the Graph.
     *
     * @return  GraphTraversal object performing traversal for the Graph.
     */
    public GraphTraversal getTraversal( ) {
        return this.traversal;
    }

    /**
     * Sets the graph traversal algorithm to be used
     *
     * @param traversal A concrete implementation of the GraphTraversal object.
     */
    public void setTraversal( GraphTraversal traversal ){
        this.traversal = traversal;
    }

    /**
     * Determines if two vertices are connected
     *
     * @param v1 starting Vertex for the path
     * @param v2 ending Vertex for the path
     * @return true if v1 and v2 are connected.
     */
    public boolean isConnected( Vertex v1, Vertex v2 ){
        Set connectedsetv1 = this.getConnectedSet( v1 );

        if( connectedsetv1.contains( v2 ))
            return true;
        else
            return false;
    }

    /**
     * Returns the degree of the graph, which is simply the highest degree
     * of all the graph's vertices.
     *
     * @return An int indicating the degree of the graph.
     */
    public int getDegree( ) {
        Vertex  v;
        Collection set;

        set = getVertexSet();
        if( set.size() > 0 ){
            v = (Vertex) Collections.max( set, new Comparator() {
                    public int compare( Object obj1, Object obj2 ) {
                        Vertex v1 = (Vertex) obj1, v2 = (Vertex) obj2;
                        int    countv1 = getDegree( v1 );
                        int    countv2 = getDegree( v2 );

                        if( countv1 < countv2 ) return -1;
                        if( countv1 > countv2 ) return 1;
                        else return 0;
                    }
                    public boolean equals( Object objcomparator ) {
                        return objcomparator.equals( this );
                    }
                }
                );
            return this.getEdges( v ).size();
        }
        else
            return 0;
    }

    /**
     * Returns the degree of the vertex, which is simply the number of edges
     * of the vertex.
     *
     * @return  The degree of the vertex.
     */
    public int getDegree( Vertex v ) {
        return this.getEdges( v ).size();
    }

    public void addListener(GraphListener listener) {
        listenerList.add(listener);
    }

    public void removeListener(GraphListener listener) {
        listenerList.remove(listener);
    }

    // Adaptor classes which adapt deprecated Listener interfaces to
    // replacement GraphListener interface.  Note that these define equals in
    // such a way that a List.remove(listener) will remove the adaptor
    // instead.

    private static abstract class ListenerAdaptor extends NullGraphListener
    {
        protected Object listener;

        protected ListenerAdaptor(Object listener)
        {
            this.listener = listener;
        }

        public boolean equals(Object obj)
        {
            if (!(obj instanceof ListenerAdaptor)) {
                return false;
            }
            return listener == ((ListenerAdaptor) obj).listener;
        }
    }

    private static class AddVertexListenerAdaptor extends ListenerAdaptor
    {
        AddVertexListenerAdaptor(GraphAddVertexListener listener)
        {
            super(listener);
        }

        public void afterVertexAdded(GraphAddVertexEvent event)
        {
            ((GraphAddVertexListener) listener).vertexAdded(event);
        }
    }

    private static class RemoveVertexListenerAdaptor extends ListenerAdaptor
    {
        RemoveVertexListenerAdaptor(GraphRemoveVertexListener listener)
        {
            super(listener);
        }

        public void beforeVertexRemoved(GraphRemoveVertexEvent event)
        {
            ((GraphRemoveVertexListener) listener).vertexRemoved(event);
        }
    }

    private static class AddEdgeListenerAdaptor extends ListenerAdaptor
    {
        AddEdgeListenerAdaptor(GraphAddEdgeListener listener)
        {
            super(listener);
        }

        public void afterEdgeAdded(GraphAddEdgeEvent event)
        {
            ((GraphAddEdgeListener) listener).edgeAdded(event);
        }
    }

    private static class RemoveEdgeListenerAdaptor extends ListenerAdaptor
    {
        RemoveEdgeListenerAdaptor(GraphRemoveEdgeListener listener)
        {
            super(listener);
        }

        public void beforeEdgeRemoved(GraphRemoveEdgeEvent event)
        {
            ((GraphRemoveEdgeListener) listener).edgeRemoved(event);
        }
    }


    /**
     * Adds a GraphAddVertexListener to the Graph's internal List of
     * GraphAddVertexListeners so that when a new Vertex is added,
     * all registered GraphAddVertedListeners are notified of the event.
     *
     * @param listener GraphAddVertexListener you want registered or be
     * notified when a new Vertex is added
     * @see salvo.jesus.graph.GraphAddVertexListener
     * @see #removeGraphAddVertexListener( GraphAddVertexListener )
     */
    public void addGraphAddVertexListener( GraphAddVertexListener listener ) {
        addListener(new AddVertexListenerAdaptor(listener));
    }

    /**
     * Adds a GraphAddEdgeListener to the Graph's internal List of
     * GraphAddEdgeListeners so that when a new Edge is added,
     * all registered GraphAddEdgeListeners are notified of the event.
     *
     * @param listener GraphAddEdgeListener you want registered or be notified
     * when a new Edge is added
     * @see salvo.jesus.graph.GraphAddEdgeListener
     * @see #removeGraphAddEdgeListener( GraphAddEdgeListener )
     */
    public void addGraphAddEdgeListener( GraphAddEdgeListener listener ) {
        addListener(new AddEdgeListenerAdaptor(listener));
    }

    /**
     * Adds a GraphRemoveEdgeListener to the Graph's internal List of
     * GraphRemoveEdgeListeners so that when an Edge is removed,
     * all registered GraphRemoveEdgeListeners are notified of the event.
     *
     * @param listener GraphRemoveEdgeListener you want registered or be
     * notified when an Edge is removed
     * @see salvo.jesus.graph.GraphRemoveEdgeListener
     * @see #removeGraphRemoveEdgeListener( GraphRemoveEdgeListener )
     */
    public void addGraphRemoveEdgeListener( GraphRemoveEdgeListener listener ) {
        addListener(new RemoveEdgeListenerAdaptor(listener));
    }

    /**
     * Adds a GraphRemoveVertexListener to the Graph's internal List of
     * GraphRemoveVertexListeners so that when a Vertex is removed,
     * all registered GraphRemoveVertexListeners are notified of the event.
     *
     * @param listener GraphRemoveVertexListener you want registered or be
     * notified when a Vertex is removed
     * @see salvo.jesus.graph.GraphRemoveVertexListener
     * @see #removeGraphRemoveVertexListener( GraphRemoveVertexListener )
     */
    public void addGraphRemoveVertexListener( GraphRemoveVertexListener listener ) {
        addListener(new RemoveVertexListenerAdaptor(listener));
    }

    /**
     * Removes a GraphAddVertexListener from the Graph's internal List of
     * GraphAddVertexListeners.
     *
     * @param listener GraphAddVertexListener you no longer want registered or
     * be notified when a Vertex is added
     * @see salvo.jesus.graph.GraphAddVertexListener
     * @see #addGraphAddVertexListener( GraphAddVertexListener )
     */
    public void removeGraphAddVertexListener( GraphAddVertexListener listener ) {
        removeListener(new AddVertexListenerAdaptor(listener));
    }

    /**
     * Removes a GraphAddEdgeListener from the Graph's internal List of
     * GraphAddEdgeListeners.
     *
     * @param listener GraphAddEdgeListener you no longer want registered or be
     * notified when an Edge is added
     * @see salvo.jesus.graph.GraphAddEdgeListener
     * @see #addGraphAddEdgeListener( GraphAddEdgeListener )
     */
    public void removeGraphAddEdgeListener( GraphAddEdgeListener listener ) {
        removeListener(new AddEdgeListenerAdaptor(listener));
    }

    /**
     * Removes a GraphRemoveEdgeListener from the Graph's internal List of
     * GraphRemoveEdgeListeners.
     *
     * @param listener GraphRemoveEdgeListener you no longer want registered or
     * be notified when an Edge is removed
     * @see salvo.jesus.graph.GraphRemoveEdgeListener
     * @see #addGraphRemoveEdgeListener( GraphRemoveEdgeListener )
     */
    public void removeGraphRemoveEdgeListener( GraphRemoveEdgeListener listener ) {
        removeListener(new RemoveEdgeListenerAdaptor(listener));
    }

    /**
     * Removes a GraphRemoveVertexListener from the Graph's internal List of
     * GraphRemoveVertexListeners.
     *
     * @param listener GraphRemoveVertexListener you no longer want registered
     * or be notified when a Vertex is removed
     * @see salvo.jesus.graph.GraphRemoveVertexListener
     * @see #addGraphRemoveVertexListener( GraphRemoveVertexListener )
     */
    public void removeGraphRemoveVertexListener( GraphRemoveVertexListener listener ) {
        removeListener(new RemoveVertexListenerAdaptor(listener));
    }

    /**
     * Returns a String representation of the Graph. The string returned in the
     * form: "Vertices: " + getVertexSet().toString() + "\n " + "Edges: " +
     * getEdgeSet().toString()
     *
     * @return	String representation of the Graph
     */
    public String toString() {
        return "Vertices: " + getVertexSet().toString() + "\n " +
            "Edges: " + getEdgeSet().toString();
    }
}

