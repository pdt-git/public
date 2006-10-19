package salvo.jesus.graph.algorithm;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import salvo.jesus.graph.Graph;
import salvo.jesus.graph.NullVisitor;
import salvo.jesus.graph.Vertex;
import salvo.jesus.graph.Visitor;
import salvo.jesus.util.EmptyQueueException;
import salvo.jesus.util.Queue;

/**
 * A concrete subclass of GraphTraversal that uses breadth-first search
 * in traversing a graph. Note that the traverse() method will only
 * traverse the connected set to which the Vertex the traversal will start at belongs.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class BreadthFirstTraversal extends GraphTraversal {
    /**
     * Queue of vertices in the order they should be visited.
     */
    private Queue queue;

    /**
     * Set of vertices which have already been seen; this is the
     * union of visited and queue.
     */
    private Set seen;

    /**
     * Creates a BreadthFirstTraversal object
     */
    public BreadthFirstTraversal( Graph graph ) {
        super( graph );
        this.queue = new Queue();
    }

    public int traverse(Vertex startat, List visited, Visitor visitor) {
        Vertex  next;
        Vertex  adjacent;
        List    adjacentVertices;
        Iterator  iterator;

        // mark all pre-visited vertices as seen
        seen = new HashSet(visited);
        
        // Put the starting vertex in the queue
        this.queue.put( startat );
        seen.add(startat);

        try {
            do {
                // Get the next vertex in the queue and add it to the visited
                next = (Vertex) this.queue.get();
                visited.add( next );

                // Exit if the visitor tells us so
                if( !visitor.visit( next )) {
                    clear();
                    return TERMINATEDBYVISITOR;
                }

                // Get all of its adjacent vertices and put them in the queue
                // only if it has not been visited and it has not been queued
                adjacentVertices = this.graph.getAdjacentVertices( next );
                iterator = adjacentVertices.iterator();
                while( iterator.hasNext()) {
                    adjacent = (Vertex) iterator.next();
                    if(seen.add(adjacent)) {
                        this.queue.put( adjacent );
                    }
                }

            } while( !this.queue.isEmpty() );
        }
        // This should not happen, but catch it anyway as it is required,
        // but do nothing.
        catch( EmptyQueueException e ) {}
        finally {
            return OK;
        }
    }
    
    private void clear()
    {
        queue.clear();
        seen = null;
    }
    
    public List traverse(Vertex startat, Visitor visitor) {
        List    visited = new ArrayList( 10 );

        this.traverse( startat, visited, visitor );
        return visited;
    }

    public List traverse(Vertex startat) {
        List    visited = new ArrayList( 10 );

        this.traverse( startat, visited, new NullVisitor() );
        return visited;
    }
}
