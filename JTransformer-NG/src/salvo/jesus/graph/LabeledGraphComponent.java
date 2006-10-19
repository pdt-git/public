package salvo.jesus.graph;

/**
 * A LabeledGraphComponent represents a Vertex or Edge with a label.
 *
 * @author John V. Sichi
 */
public interface LabeledGraphComponent extends GraphComponent
{
    /**
     * @return does this component have a label?  In some graphs, labeling may
     * be optional, in which case this method may return true for some or all
     * components.
     */
    public boolean hasLabel();

    /**
     * @return the label associated with this component; if hasLabel() returns
     * false, the result of getLabel() is undefined
     */
    public String getLabel();

    /**
     * Set the label on this component.  In many cases, attempting to reassign
     * a label after one has already been set will cause an assertion failure.
     *
     * @param label the new label to assign to the component
     */
    public void setLabel(String label);

    /**
     * Notify this component that it is being added to a graph.
     */
    //public void notifyAddedToGraph();
}

// End LabeledGraphComponent.java
