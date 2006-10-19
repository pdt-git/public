package salvo.jesus.graph;

import java.io.*;

/**
 * A interface for a visitor in the Visitor Pattern.
 */

public interface Visitor extends Serializable {

  public boolean visit( Vertex vertexToVisit );
}

