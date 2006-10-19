package salvo.jesus.graph;

/**
 * Exception superclass thrown from methods of graphs.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class GraphException extends Exception {

    public GraphException() {
        super();
    }

    public GraphException( String msg ) {
        super( msg );
    }
}