package salvo.jesus.graph;

/**
 * An interface that abstracts a path in a graph. In this context,
 * a path is a <b>consecutive</b> sequence of vertices from one vertex
 * to another connected by edges.
 * <p>
 * The important word here is <b>consecutive</b>. Thus, implementations
 * are required to make sure that if an <tt>Edge</tt> is added to a
 * <tt>Path</tt>, that the <tt>Edge</tt> being added is between
 * the last <tt>Vertex</tt> in the path to a new <tt>Vertex</tt>.
 * If a <tt>Vertex</tt> is being added, that am <tt>Edge</tt> is
 * created between the last <tt>Vertex</tt> in the <tt>Path</tt> to
 * the new <tt>Vertex</tt> being added.
 * <p>
 * If an acyclic path is required, then implement the <tt>AcyclicPath</tt>
 * interface instead.
 *
 * @author  Jesus M. Salvo Jr.
 */

public interface Path extends Graph {

    /**
     * Returns the first <tt>Vertex</tt> in the <tt>Path</tt>.
     */
    public Vertex getFirstVertex();

    /**
     * Returns the last <tt>Vertex</tt> in the <tt>Path</tt>.
     */
    public Vertex getLastVertex();

    /**
     * Removes the last Vertex that was added in the <tt>Path</tt>.
     */
    public void remove() throws Exception;

}