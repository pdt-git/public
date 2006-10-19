package salvo.jesus.graph;

/**
 * The factory for creating Vertices and Edges in a <tt>WeightedGraphImpl</tt> class.
 *
 * @author  Jesus M. Salvo jr.
 */

public class WeightedGraphImplFactory implements GraphFactory {

    public WeightedGraphImplFactory() {}

    public Vertex createVertex() {
        return new VertexImpl( "New Vertex" );
    }

    public Edge createEdge( Vertex v1, Vertex v2 ) {
        return new WeightedEdgeImpl( v1, v2, 0 );
    }
}