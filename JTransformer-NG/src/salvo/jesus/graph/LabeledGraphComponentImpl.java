package salvo.jesus.graph;
import java.util.*;

/**
 * LabeledGraphComponentImpl provides a default implementation for
 * LabeledGraphComponent.  Once a label is assigned, it can never be reassigned
 * for the lifetime of the component.
 *
 * @author John V. Sichi
 */
public class LabeledGraphComponentImpl
    implements LabeledGraphComponent, Comparable
{
    /**
     * Define a null label string for components which were
     * not assigned a label.  This allows us to prevent attempts to
     * assign a label after the component has been added to a graph.
     */
    private static final String g_noLabel = "<unlabeled>";

    private String m_label;

    public int compareTo(Object o)
    {
        if (m_label == null) {
            return 0;
        }
        LabeledGraphComponentImpl other = (LabeledGraphComponentImpl) o;
        return m_label.compareTo(other.m_label);
    }

    //public void notifyAddedToGraph()
    //{
    //    if (m_label == null) {
    //        m_label = g_noLabel;
    //    }
    //}

    public void setLabel(String label)
    {
        //if (m_label != null) {
        //    throw new IllegalStateException("label cannot be reassigned");
        //}
        m_label = label;
    }

    public boolean hasLabel()
    {
        return (m_label != null) && (m_label != g_noLabel);
    }

    public String getLabel()
    {
        return m_label;
    }

    public String toString()
    {
        return m_label;
    }
}

// End LabeledGraphComponentImpl.java
