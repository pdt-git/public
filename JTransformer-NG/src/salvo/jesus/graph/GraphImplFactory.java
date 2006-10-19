package salvo.jesus.graph;

/**
 * The factory for creating Vertices and Edges in a <tt>GraphImpl</tt> class.
 *
 * @author  Jesus M. Salvo jr.
 */

public class GraphImplFactory implements GraphFactory {

    public GraphImplFactory() {}

    public Vertex createVertex() {
        return new VertexImpl( "New Vertex" );
    }

    public Edge createEdge( Vertex v1, Vertex v2 ) {
        return new EdgeImpl( v1, v2 );
    }
}