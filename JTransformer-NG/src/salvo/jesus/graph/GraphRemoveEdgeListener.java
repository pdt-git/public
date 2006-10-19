package salvo.jesus.graph;

import java.util.*;
import java.io.*;

/**
 * The listener interface for receiving notification when an Edge is
 * is about to be removed from a Graph.
 *
 * Implementations of this interface should be registered with a Graph,
 * via the Graph's addGraphRemoveEdgeListener(), to be notified
 * when an Edge is about to be removed from the Graph.
 *
 * @author		Jesus M. Salvo Jr.
 *
 * @deprecated Use GraphListener instead
 * @see GraphListener
 */

public interface GraphRemoveEdgeListener extends EventListener, Serializable {

  /**
    * Once implementations of this interface are registered with a Graph,
    * this method is automatically called whenever an Edge is about to be
    * removed from the Graph object.
    *
    * @param	e		GraphRemoveEdgeEvent object that also specifies the Edge
    * that is about to be removed from the graph
    */
  public void edgeRemoved( GraphRemoveEdgeEvent e );
}
