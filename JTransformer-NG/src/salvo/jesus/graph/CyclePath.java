package salvo.jesus.graph;

/**
 * An empty interface that denotes a <tt>CyclePath</tt>. This is a <tt>SimplePath</tt>
 * but the first and last vertex being the same.
 *
 * @author  Jesus M. Salvo Jr.
 */

public interface CyclePath extends SimplePath {

    /**
     * Method to be called indicating that the <tt>Path</tt> has been
     * fully specified. Implementations of this method should therefore
     * check that the <tt>Path</tt> is indeed a <tt>Cycle</tt>.
     */
    public void closeCycle() throws IllegalPathException;

}