package salvo.jesus.graph;

/**
 * The factory for creating Vertices and Edges in a <tt>DirectedGraphImpl</tt> class.
 *
 * @author  Jesus M. Salvo jr.
 */

public class DirectedGraphImplFactory implements GraphFactory {

    public DirectedGraphImplFactory() {}

    public Vertex createVertex() {
        return new VertexImpl( "New Vertex" );
    }

    public Edge createEdge( Vertex v1, Vertex v2 ) {
        return new DirectedEdgeImpl( v1, v2 );
    }
}