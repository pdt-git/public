package salvo.jesus.graph;

/**
 * @author  Jesus M. Salvo Jr.
 */

public class NoSuchVertexException extends GraphException {

    public NoSuchVertexException() {
        super();
    }

    public NoSuchVertexException( String msg ) {
        super( msg );
    }
}