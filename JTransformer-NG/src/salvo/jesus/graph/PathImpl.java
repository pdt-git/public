package salvo.jesus.graph;
import salvo.jesus.graph.listener.*;

/**
 * An implementation of the <tt>Path</tt> interface that is a non-simple path.
 * A non-simple path is a <tt>Path</tt> whereby vertices maybe repeated.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class PathImpl extends AbstractPathImpl {
    /**
     * Creates an instance of <tt>PathImpl</tt>.
     */
    public PathImpl() {
        PathListener listener = new PathListener(this);
        setListener(listener);
        setTraversal(listener.getTraversal());
    }
}
