package salvo.jesus.graph.algorithm;

import salvo.jesus.graph.*;
import java.util.*;

/**
 * A concrete subclass of GraphTraversal that uses depth-first search in
 * traversing a graph. Note that the traverse() method will only traverse the
 * connected set to which the Vertex the traversal will start at belongs.
 *
 * @author  Jesus M. Salvo Jr.
 */
public class DepthFirstGraphTraversal extends GraphTraversal {
    /**
     * Stack of vertices in the order they should be visited.
     */
    private Stack stack;

    /**
     * Set of vertices which have already been seen; this is the
     * union of visited and stack.
     */
    private Set seen;

    /**
     * Creates a DepthFirstGraphTraversal object.
     */
    public DepthFirstGraphTraversal( Graph graph ) {
        super( graph );
        this.stack = new Stack();
    }

    public int traverse( Vertex startat, List visited, Visitor visitor ) {
        Vertex  next;
        Vertex  adjacent;
        List    adjacentVertices;
        Iterator  iterator;

        // mark all pre-visited vertices as seen
        seen = new HashSet(visited);
        
        // Push the starting vertex onto the stack
        this.stack.push( startat );
        seen.add(startat);

        do {
            // Get the next vertex in the queue and add it to visited
            next = (Vertex) this.stack.pop();
            visited.add( next );

            // Exit if the visitor tells us so
            if( !visitor.visit( next )) {
                clear();
                return TERMINATEDBYVISITOR;
            }

            // Get all of its adjacent vertices and push them onto the stack
            // only if it has not been visited and it has not been stacked
            adjacentVertices = getAdjacentVertices( next );
            iterator = adjacentVertices.iterator();
            while( iterator.hasNext()) {
                adjacent = (Vertex) iterator.next();
                if (seen.add(adjacent)) {
                    this.stack.push( adjacent );
                }
            }

        } while( !this.stack.isEmpty() );
        clear();
        return OK;
    }

    private void clear()
    {
        stack.clear();
        seen = null;
    }

    /**
     * Get the vertices adjacent to v.  This is overridden by
     * subclasses such as DepthFirstDirectedGraphTraversal.
     */
    protected List getAdjacentVertices(Vertex v)
    {
        return graph.getAdjacentVertices(v);
    }
    
    public List traverse( Vertex startat ) {
        return this.traverse( startat, new NullVisitor());
    }

    public List traverse( Vertex startat, Visitor visitor ) {
        List  visited = new ArrayList( 10 );

        this.traverse( startat, visited, visitor );
        return visited;
    }
}
