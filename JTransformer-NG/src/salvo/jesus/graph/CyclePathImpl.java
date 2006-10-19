package salvo.jesus.graph;
import salvo.jesus.graph.listener.*;

/**
 * Implementation of <tt>CyclePath</tt> that ensures that the first
 * and last vertices in the <tt>Path</tt> forms a cycle.
 *
 * @author  Jesus M. Salvo Jr.
 */
public class CyclePathImpl extends AbstractPathImpl implements CyclePath {

    /**
     * Reference to whether closeCycle has been called.
     */
    private boolean isPathClosed;
    
    /**
     * Creates an instance of <tt>CyclePathImpl</tt>.
     */
    public CyclePathImpl() {
        super();
        setListener(new CyclePathListener(this,false));
    }

    /**
     * Method to be called indicating that the <tt>Path</tt> has been
     * fully specified. Implementations of this method should therefore
     * check that the <tt>Path</tt> is indeed a <tt>Cycle</tt>.
     *
     * @throws  IllegalPathException    Thrown when the CyclePath is already
     * closed or is being closed but is not a cycle.
     */
    public void closeCycle() throws IllegalPathException {
        // If path is already closed, throw Exception
        if( this.isPathClosed ) {
            throw new IllegalPathException(
                "CyclePath is already closed.");
        }

        // If last Vertex added to the Path did not form a cycle,
        // then we do not have a cycle path. Raise exception.
        if (getLastVertex() != getFirstVertex()) {
            throw new IllegalPathException(
                "CyclePath is being closed but is not a cycle." );
        }

        this.isPathClosed = true;

        // become immutable now that cycle is closed
        new ImmutableGraphListener(this);
    }

}
