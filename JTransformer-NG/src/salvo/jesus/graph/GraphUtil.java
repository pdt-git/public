package salvo.jesus.graph;
import java.util.*;
import java.io.*;
import salvo.jesus.graph.algorithm.*;

/**
 * Graph-level utility functions.  This is a class with all static methods for
 * utilities that aren't quite big enough to deserve their own classes, and
 * aren't commonly used enough to merit becoming part of one of the Graph
 * interfaces.
 *
 * @author jvs@radik.com
 * @version $Id$
 */
public class GraphUtil
{
    /**
     * Merge two graphs.  The result is disconnected.  The results are
     * undefined if the two graphs already share Vertices.  (Note that if
     * Graph Vertex and Edge collections were Sets instead, this would not be
     * the case; the result would be the connected union of the two graphs.)
     *
     * @param gDst one of the inputs, and also the output (receives the merged
     * result)
     *
     * @param gSrc the other input (unmodified by this operation)
     */
    public static void mergeInto(Graph gDst,Graph gSrc)
    {
        // first pass:  copy all the vertices
        Iterator vertexIter = gSrc.getVerticesIterator();
        while (vertexIter.hasNext()) {
            Vertex v = (Vertex) vertexIter.next();
            addVertex(gDst,v);
        }

        // second pass:  copy all the edges
        vertexIter = gSrc.getVerticesIterator();
        while (vertexIter.hasNext()) {
            Vertex v = (Vertex) vertexIter.next();
            Iterator edgeIter = gSrc.getEdges(v).iterator();
            while (edgeIter.hasNext()) {
                Edge edge = (Edge) edgeIter.next();
                // this test assures that each edge is added exactly once
                if (edge.getVertexA() == v) {
                    addEdge(gDst,edge);
                }
            }
        }
    }

    /**
     * Add a Vertex to a Graph, converting Exceptions into RuntimeExceptions.
     * For use when you know this operation cannot possibly result in an
     * Exception, and don't want to litter your code with try/catch everywhere
     * you modify a Graph.
     *
     * @param g the graph to be updated
     *
     * @param v the vertex to add
     */
    public static void addVertex(Graph g,Vertex v)
    {
        try {
            g.add(v);
        } catch (Exception ex) {
            throwUnexpectedException(ex);
        }
    }

    /**
     * Add an Edge to a Graph, converting Exceptions into RuntimeExceptions.
     * For use when you know this operation cannot possibly result in an
     * Exception, and don't want to litter your code with try/catch everywhere
     * you modify a Graph.
     *
     * @param g the graph to be updated
     *
     * @param e the edge to add
     */
    public static void addEdge(Graph g,Edge e)
    {
        try {
            g.addEdge(e);
        } catch (Exception ex) {
            throwUnexpectedException(ex);
        }
    }

    /**
     * Add an edge to a Graph, converting Exceptions into RuntimeExceptions.
     * For use when you know this operation cannot possibly result in an
     * Exception, and don't want to litter your code with try/catch everywhere
     * you modify a Graph.
     *
     * @param g the graph to be updated
     *
     * @param v1 one of the vertices (the source if g is a DirectedGraph)
     *
     * @param v2 the other vertex (the sink if g is a DirectedGraph)
     */
    public static void addEdge(Graph g,Vertex v1,Vertex v2)
    {
        try {
            g.addEdge(v1,v2);
        } catch (Exception ex) {
            throwUnexpectedException(ex);
        }
    }

    /**
     * Remove a Vertex from a Graph, converting Exceptions into
     * RuntimeExceptions.  For use when you know this operation cannot possibly
     * result in an Exception, and don't want to litter your code with
     * try/catch everywhere you modify a Graph.
     *
     * @param g the graph to be updated
     *
     * @param v the vertex to remove
     */
    public static void removeVertex(Graph g,Vertex v)
    {
        try {
            g.remove(v);
        } catch (Exception ex) {
            throwUnexpectedException(ex);
        }
    }
    
    /**
     * Remove an Edge from a Graph, converting Exceptions into
     * RuntimeExceptions.  For use when you know this operation cannot possibly
     * result in an Exception, and don't want to litter your code with
     * try/catch everywhere you modify a Graph.
     *
     * @param g the graph to be updated
     *
     * @param e the edge to remove
     */
    public static void removeEdge(Graph g,Edge e)
    {
        try {
            g.removeEdge(e);
        } catch (Exception ex) {
            throwUnexpectedException(ex);
        }
    }

    private static void throwUnexpectedException(Exception ex)
    {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        ex.printStackTrace(pw);
        pw.flush();
        throw new RuntimeException(
            "unexpected graph mutator exception "+
            ex.getClass().getName()+": "+ex.getMessage()+"\n"+sw.toString());
    }

    /**
     * Contract two vertices within a graph.  The vertices need not be
     * already connected.
     *
     * @param g the graph containing the vertices
     *
     * @param vRemain the vertex which will remain in the graph after
     * contraction
     *
     * @param vRemove the other vertex which will be removed as a result
     * of the contraction
     *
     * @param rule callback object used to decide how to deal with edges;
     * if null, no edges are treated as self-loops, and the graph's factory is
     * used for cloning edges
     */
    public static void contractVertices(
        Graph g,Vertex vRemain,Vertex vRemove,EdgeContractionRule rule)
    {
        if (vRemove == vRemain) {
            // that's easy enough...maybe should filter any self-loops
            // through rule as well?
            return;
        }
        // transfer all of vRemove's edges to vRemain
        List edgeList = g.getEdges(vRemove);
        if (edgeList == null) {
            edgeList = Collections.EMPTY_LIST;
        }
        Iterator edges = edgeList.iterator();
        while (edges.hasNext()) {
            Edge edge = (Edge) edges.next();
            Vertex opposite = edge.getOppositeVertex(vRemove);
            if (opposite == vRemain) {
                if ((rule == null) || !rule.shouldBeSelfLoop(edge)) {
                    continue;
                } else {
                    // below will create a self-loop
                }
            }
            
            if (opposite == vRemove) {
                // there was already a self-loop
                if ((rule != null) && rule.shouldBeSelfLoop(edge)) {
                    // transfer the accepted self-loop to vRemain
                    opposite = vRemain;
                } else {
                    // this rejected self-loop will go away when
                    // we delete vRemove at the end
                }
            }

            // preserve direction in case it's relevant (assumes vertexA is
            // always the source for a DirectedEdge)
            Edge newEdge;
            Vertex vA,vB;
            if (vRemove == edge.getVertexA()) {
                vA = vRemain;
                vB = opposite;
            } else {
                vA = opposite;
                vB = vRemain;
            }
            if (rule != null) {
                newEdge = rule.cloneEdge(edge,vA,vB);
            } else {
                newEdge = g.getGraphFactory().createEdge(vA,vB);
            }
            addEdge(g,newEdge);
        }

        // now lose vRemove
        removeVertex(g,vRemove);
    }

    /**
     * @return true if the given DirectedGraph contains any cycles
     */
    public static boolean detectCycles(DirectedGraph graph)
    {
        Iterator vertexIter = graph.getVerticesIterator();
        while (vertexIter.hasNext()) {
            Vertex v = (Vertex) vertexIter.next();
            if (graph.isCycle(v)) {
                return true;
            }
        }
        return false;
    }
    /**
     * EdgeContractionRule is a callback for the contractVertices method.
     */
    public static interface EdgeContractionRule
    {
        /**
         * Called by contractVertices to decide whether a given edge
         * encountered during contraction should be turned into a self-loop
         * on the contracted vertex.
         */
        public boolean shouldBeSelfLoop(Edge e);

        /**
         * Called by contractVertices to clone an edge while changing its
         * incidence.
         */
        public Edge cloneEdge(Edge e,Vertex vA,Vertex vB);
    }
}

// End GraphUtil.java
