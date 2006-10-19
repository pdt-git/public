package salvo.jesus.graph;
import salvo.jesus.graph.listener.*;

/**
 * Implementation of <tt>SimplePath</tt> interface guaranteeing that the
 * path is simple, meaning no <tt>Vertex</tt> is repeated in the path.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class SimplePathImpl extends AbstractPathImpl implements SimplePath {
    /**
     * Creates an instance of <tt>SimplePathImpl</tt>.
     */
    public SimplePathImpl() {
        setListener(new SimplePathListener(this));
    }

}
