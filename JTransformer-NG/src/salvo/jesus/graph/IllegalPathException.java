package salvo.jesus.graph;

/**
 * Thrown whenever a method in <tt>Path</tt> is called that will result
 * to a violation of the definition of a <tt>Path</tt>.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class IllegalPathException extends GraphException {

    public IllegalPathException() {
        super();
    }

    public IllegalPathException( String msg ) {
        super( msg );
    }
}