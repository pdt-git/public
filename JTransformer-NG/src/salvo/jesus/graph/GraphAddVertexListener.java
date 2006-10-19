package salvo.jesus.graph;

import java.util.*;
import java.io.*;

/**
 * The listener interface for receiving notification when a Vertex is
 * is added to a Graph.
 *
 * Implementations of this interface should be registered with a Graph,
 * via the Graph's addGraphAddVertexListener(), to be notified
 * when a Vertex is added to the Graph.
 *
 * @author		Jesus M. Salvo Jr.
 *
 * @deprecated Use GraphListener instead
 * @see GraphListener
 */

public interface GraphAddVertexListener extends EventListener, Serializable {

  /**
    * Once implementations of this interface are registered with a Graph,
    * this method is automatically called whenever a Vertex is added to the
    * Graph object.
    *
    * @param	e		GraphAddVertexEvent object that also specifies the Vertex that was added
    * to the graph
    */
  public void vertexAdded( GraphAddVertexEvent e );
}
