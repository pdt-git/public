package salvo.jesus.graph.adaptor;
import salvo.jesus.graph.*;
import salvo.jesus.graph.algorithm.*;
import salvo.jesus.graph.listener.*;
import java.util.*;

/**
 * DirectedAcyclicGraphAdaptor allows an underlying DirectedGraph to be viewed
 * as a DirectedAcyclicGraph.  This is useful when you know a DirectedGraph
 * contains no cycles, and want to use it in an algorithm that expects a DAG.
 * The DirectedGraph has the DAG property enforced for as long as
 * the adaptor is attached.
 *
 * @author John V. Sichi
 */
public class DirectedAcyclicGraphAdaptor
    extends DirectedGraphDelegator implements DirectedAcyclicGraph
{
    /**
     * Delegate object to handle topological sorting
     */
    private TopologicalSorting m_sorter;

    /**
     * Enforce DAG on underlying graph.
     */
    private DirectedAcyclicGraphListener m_listener;

    /**
     * Create a view of a DirectedGraph as a DAG.
     *
     * @exception CycleException if the given graph already contains cycles
     */
    public DirectedAcyclicGraphAdaptor(DirectedGraph graph)
        throws CycleException
    {
        super(graph);
        CycleDetectionAlgorithm cycleDetector =
            new CycleDetectionAlgorithmDFS(graph);
        if (cycleDetector.detectCycles()) {
            throw new CycleException();
        }
        m_listener = new DirectedAcyclicGraphListener(this);
        m_sorter = new TopologicalSorting(this);
    }

    /**
     * Destroy this adaptor, making the underlying graph free to violate the
     * DAG property in future modifications.
     */
    public void destroy()
    {
        removeListener(m_listener);
    }
    
    public List getRoot()
    {
        return m_listener.getRoot();
    }

    public List topologicalSort()
    {
        return m_sorter.traverse();
    }

    public List reverseTopologicalSort()
    {
        return m_sorter.reverseTraverse();
    }

    public List topologicalSort(Vertex startat)
    {
        return m_sorter.traverse(startat);
    }

    public List reverseTopologicalSort(Vertex startat)
    {
        return m_sorter.reverseTraverse(startat);
    }
}

// End DirectedAcyclicGraphAdaptor.java
