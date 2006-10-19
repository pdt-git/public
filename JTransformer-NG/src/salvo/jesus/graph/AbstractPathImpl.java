package salvo.jesus.graph;
import salvo.jesus.graph.listener.*;
import java.util.*;

/**
 * AbstractPathImpl is a partial implementation and common base
 * for PathImpl, SimplePathImpl, and CyclePathImpl.
 *
 * @author John V. Sichi
 * @version $Id$
 */
abstract class AbstractPathImpl extends GraphImpl implements Path
{
    private AbstractPathListener listener;

    protected void setListener(AbstractPathListener listener)
    {
        this.listener = listener;
    }
    
    /**
     * @see Path#getFirstVertex
     */
    public Vertex getFirstVertex() {
        return listener.getFirstVertex();
    }

    /**
     * @see Path#getLastVertex
     */
    public Vertex getLastVertex() {
        return listener.getLastVertex();
    }

    /**
     * Adds a Vertex into the Path.
     * <p>
     * This will call <tt>Graph.add( Vertex )</tt> only if
     * the <tt>Vertex</tt> is not yet part of the <tt>Graph</tt>.
     * Note that this will also automatically add an <tt>Edge</tt> from the
     * last <tt>Vertex</tt> that was added to the this <tt>Vertex</tt>
     * being added.
     * <p>
     * If adding this new <tt>Edge</tt> instance is not desired and you want
     * to add an existing <tt>Edge</tt> instance instead ( i.e.: from
     * an <tt>Edge</tt> in a <tt>Graph</tt> ), then you should call
     * <tt>addEdge( Edge )</tt> instead.
     *
     * @param		newvertex		Vertex to be added to the Path
     * @throws      IllegalPathException
     */
    public void add( Vertex newVertex ) throws Exception {
        if (getLastVertex() == null) {
            super.add(newVertex);
        } else {
            addEdge(getLastVertex(),newVertex);
        }
    }
    
    /**
     * Removes the last Vertex that was added in the <tt>Path</tt>.
     */
    public void remove() throws Exception {
        Vertex v = listener.getLastVertex();
        if (v == null) {
            throw new NoSuchElementException();
        }
        remove(v);
    }

    /**
     * Returns a String representation of the Path.
     */
    public String toString() {
        Iterator    iterator = traverse(getFirstVertex()).iterator();
        StringBuffer    out = new StringBuffer();
        String          arrow = "->";

        while( iterator.hasNext()) {
            out.append( iterator.next().toString() );
            if( iterator.hasNext()) {
                out.append( arrow );
            }
        }

        return out.toString();
    }
}

// End AbstractPathImpl.java
