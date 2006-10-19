package salvo.jesus.graph;

import java.util.*;
import java.io.*;

/**
 * The listener interface for receiving notification when a Vertex is
 * is about to be removed from a Graph.
 *
 * Implementations of this interface should be registered with a Graph,
 * via the Graph's addGraphRemoveVertexListener(), to be notified
 * when a Vertex object is about to be removed from the Graph.
 *
 * @author		Jesus M. Salvo Jr.
 *
 * @deprecated Use GraphListener instead
 * @see GraphListener
 */

public interface GraphRemoveVertexListener extends EventListener, Serializable {

  /**
    * Once implementations of this interface are registered with a Graph,
    * this method is automatically called whenever a Vertex is about to be
    * removed from the Graph object encapsulated by the VisualGraph object.
    *
    * @param	e		GraphRemoveVertexEvent object that also specifies the Vertex
    * that is about to be removed from the graph
    */
  public void vertexRemoved( GraphRemoveVertexEvent e );
}
