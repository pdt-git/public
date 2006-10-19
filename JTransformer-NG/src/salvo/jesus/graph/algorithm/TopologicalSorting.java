package salvo.jesus.graph.algorithm;

import java.util.*;
import salvo.jesus.graph.*;

/**
 * A concrete subclass of GraphTraversal that performs a topological sort
 * against a directed acyclic graph.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class TopologicalSorting extends GraphTraversal
{
    private DirectedAcyclicGraph dag;

    /**
     * Inner class for keeping track of reference counts.  The reference
     * count is the number of parents which have not yet been returned.  Can't
     * use Integer, since it's immutable.
     */
    private static class RefCount
    {
        int nRefs;
    }
    
    /**
     * Creates an instance of TopologicalSorting that will perform a
     * topological sort against a directed acyclic graph.
     *
     * @param dag The DirectedAcyclicGraph on which topological sorting will be
     * performed.
     */
    public TopologicalSorting( DirectedAcyclicGraph dag ) {
        super( dag );
        this.dag = dag;
    }

    /**
     * Perform a topological sort of the subgraph reachable via outgoing
     * edges from a specific vertex.  Other edges are ignored
     * for the purpose of this sort.
     *
     * @param	startat	  The Vertex to which you want to start the traversal.
     * @param	visited	  List of vertices that has been visited,
     *                  in the sequence they were visited; must be empty
     *                  initially
     * @param visitor   Visitor object to visit each vertex as they are visited.
     *                  Return value of the visitor is ignored.
     */
    public int traverse( Vertex startat, List visited, Visitor visitor) {
        // First, collect all the reachable vertices using a DFS.  Don't use
        // the passed-in visitor, because this traversal is NOT in topological
        // order yet.  TODO:  combine DFS with reference count computation
        // using a private visitor?
        DepthFirstDirectedGraphTraversal dfs =
            new DepthFirstDirectedGraphTraversal(dag);
        List vertices = dfs.traverse(startat);

        // Then, compute reference counts.  Note that reference counts here are
        // with respect to the reachable subgraph, not with respect to the
        // entire DAG.
        Map vertexMap = new HashMap(vertices.size());
        Iterator vertexIter = vertices.iterator();
        while (vertexIter.hasNext()) {
            Vertex v = (Vertex) vertexIter.next();
            Iterator children = dag.getOutgoingAdjacentVertices(v).iterator();
            while (children.hasNext()) {
                Vertex child = (Vertex) children.next();
                RefCount refCount = (RefCount) vertexMap.get(child);
                if (refCount == null) {
                    refCount = new RefCount();
                    vertexMap.put(child,refCount);
                }
                refCount.nRefs++;
            }
        }

        // We're guaranteed a single root by the above construction.
        List rootList = new ArrayList();
        rootList.add(startat);
        
        // Finally, perform the topological sort.
        traverseImpl(vertexMap,rootList,visited,visitor);
        
        return OK;
    }

    /**
     * Perform a topological sort of the subgraph reachable via outgoing
     * edges from a specific vertex.  Other edges are ignored
     * for the purpose of this sort.
     *
     * @param	startat	  The Vertex to which you want to start the traversal.
     * @param visitor   Visitor object to visit each vertex as they are visited.
     *                  Return value of the visitor is ignored.
     *
     * @return  A List of vertices in the order that they were visited.
     */
    public List traverse( Vertex startat, Visitor visitor ) {
        List    visited = new ArrayList( 10 );

        this.traverse( startat, visited, visitor );
        return visited;
    }

    /**
     * Perform a topological sort of the subgraph reachable via outgoing
     * edges from a specific vertex.  Other edges are ignored
     * for the purpose of this sort.
     *
     * @param	startat	  The Vertex to which you want to start the traversal.
     *
     * @return  A List of vertices in the order that they were visited.
     */
    public List traverse( Vertex startat ) {
        return this.traverse( startat, new NullVisitor());
    }

    /**
     * Perform a reverse topological sort of the subgraph reachable via
     * outgoing edges from a specific vertex.  Note that this does NOT traverse
     * by incoming edges, as you might expect; rather, it first
     * traverses by outgoing edges, and then simply reverses the order
     * of the vertices encountered.
     *
     * This method is not part of the GraphTraversal abstract class, but is
     * added here for convenience.
     *
     * @param	startat	  The Vertex to which you want to start the traversal.
     *
     * @return  A List of vertices in the order that they were visited.
     */
    public List reverseTraverse( Vertex startat ) {
        List sortSequence = this.traverse( startat, new NullVisitor());
        Collections.reverse( sortSequence );
        return sortSequence;
    }

    /**
     * Perform a topological sort of the entire directed acyclic graph.  Note
     * that the sequence of vertices in the return List will not distinguish
     * between connected components of the graph.
     *
     * This method is not part of the GraphTraversal abstract class, but is
     * added here for convenience.
     *
     * @return List containing the sequence of the vertices visited in the
     * entire directed acyclic graph, regardless of the connected components of
     * the graph.
     */
    public List traverse() {
        Map vertexMap = new HashMap(dag.getVerticesCount());
        List rootList = new ArrayList();

        // Perform first pass over the graph to compute RefCounts for
        // all vertices, collecting roots at the same time.
        Iterator vertexIter = dag.getVerticesIterator();
        while (vertexIter.hasNext()) {
            Vertex v = (Vertex) vertexIter.next();
            int nRefs = dag.getIncomingEdges(v).size();
            if (nRefs == 0) {
                rootList.add(v);
            } else {
                RefCount refCount = new RefCount();
                refCount.nRefs = nRefs;
                vertexMap.put(v,refCount);
            }
        }

        // Perform second pass which actually sorts.
        List visitedList = new ArrayList(vertexMap.size());
        traverseImpl(vertexMap,rootList,visitedList,new NullVisitor());
        return visitedList;
    }

    /**
     * Perform a reverse topological sort of the entire directed acyclic graph.
     * Note that the sequence of vertices in the return List will not
     * distinguish between connected components of the graph.
     *
     * This method is not part of the GraphTraversal abstract class, but is
     * added here for convenience.
     *
     * @return List containing the sequence of the vertices visited in the
     * entire directed acyclic graph, regardless of the connected components of
     * the graph.
     */
    public List reverseTraverse( ){
        List    sortSequence = this.traverse();
        Collections.reverse( sortSequence );
        return sortSequence;
    }

    /**
     * The common implementation for all traversal variations.  This method
     * operates over only the vertices in the union of vertexMap and rootList.
     *
     * @param vertexMap Map from Vertex to RefCount; once a vertex's RefCount
     * falls to 0, it's ready to be returned; this map does not include
     * roots
     *
     * @param rootList List<Vertex> representing the roots;
     * this list will be modified in place (and will be empty once
     * the algorithm completes)
     *
     * @param visitedList List<Vertex> which receives the total ordering
     * imposed by the topological sort
     *
     * @param visitor the visitor to notify as each vertex is traversed
     */
    private void traverseImpl(
        Map vertexMap,List rootList,List visitedList,Visitor visitor)
    {
        if (!visitedList.isEmpty()) {
            throw new IllegalArgumentException(
                "initial visited List must be empty");
        }

        int nVertices = vertexMap.size() + rootList.size();
        
        while (!rootList.isEmpty()) {
            Vertex v = (Vertex) rootList.remove(rootList.size() - 1);
            visitedList.add(v);
            // Let the visitor object visit this vertex. Ignore return value.
            visitor.visit(v);
            Iterator childIter = dag.getOutgoingAdjacentVertices(v).iterator();
            while (childIter.hasNext()) {
                Vertex child = (Vertex) childIter.next();
                RefCount refCount = (RefCount) vertexMap.get(child);
                refCount.nRefs--;
                if (refCount.nRefs == 0) {
                    // all parents processed for this child; it's now a root
                    rootList.add(child);
                }
            }
        }
        if (visitedList.size() != nVertices) {
            // Something's seriously wrong, since every vertex is supposed to
            // be returned.  The "DAG" must have had a cycle.
            throw new IllegalArgumentException("cycle detected in DAG");
        }
    }
}
