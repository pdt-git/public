package salvo.jesus.graph.listener;
import salvo.jesus.graph.*;

/**
 * AbstractPathListener represents a common interface for
 * PathListener, SimplePathListener, and CyclePathListener.
 *
 * @author John V. Sichi
 * @version $Id$
 */
public interface AbstractPathListener extends GraphListener
{
    public Vertex getFirstVertex();
    public Vertex getLastVertex();
}

// End AbstractPathListener.java
