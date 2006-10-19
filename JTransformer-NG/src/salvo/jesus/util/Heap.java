package salvo.jesus.util;

import java.util.*;
import java.io.*;

/**
 * The Heap class implements a heap data structure, also called a priority queue.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class Heap implements Serializable {

  /**
   * To hold the actual storage of the Heap class.
   */
  List  binarytree;

  /**
   * HeapNodeComparator to compare two heap nodes in the heap.
   */
  HeapNodeComparator  comparator;

  /**
   * Creates an instance of a Heap. Using this constructor will make use
   * of the default HeapNodeComparator. The HeapNodeConstructor created
   * will have priorities sorted in such a way that priorities that are
   * numerically higher are at the top of the heap.
   *
   */
  public Heap(  ) {
    this.binarytree = new ArrayList( 10 );
    this.comparator = new HeapNodeComparator( -1 );
  }

  /**
   * Creates an instance of a Heap, with a specified HeapNodeComparator.
   * This way, you can control if nodes with numerically higher priorities are
   * at the top or bottom of the heap.
   *
   * To have nodes with numerically lower priorities at the top of the heap,
   * create a HeapNodeComparator with a constructor parameter that is > 0.
   *
   * @param comapartor  The HeapNodeComparator object to be used in comparing
   * the priorities of the nodes of the heap.
   */
  public Heap( HeapNodeComparator comparator ) {
    this.binarytree = new ArrayList( 10 );
    this.comparator = comparator;
  }

  /**
   * Add a new item into the heap
   */
  public void insert( HeapNode node ) {
    int   preinsertsize = binarytree.size();

    this.binarytree.add( node );

    // Fix up the heap due to the addition of a new node in the heap.
    // Note that we do not add 1, because a List is 0-based and the
    // method expects a 0-based index.
    this.upHeap( preinsertsize );
  }

  /**
   * Remove the item with the highest priority from the heap.
   */
  public HeapNode remove() {
    // Get the node at the top of the heap
    HeapNode  topnode = (HeapNode) this.binarytree.get( 0 );
    // Get the node at the bottom of the heap
    HeapNode  endnode = (HeapNode) this.binarytree.get( this.binarytree.size() - 1 );

    // Put the node that is at the bottom to the top of the heap.
    this.binarytree.set( 0, endnode );
    // Then reduce the size of the heap
    this.binarytree.remove( this.binarytree.size() - 1 );

    // Fix up the heap due to the removal of a node in the heap
    // and due to the moving of the last node in the heap to the top of the heap.
    // Note that we do not add 1, because a List is 0-based and the
    // method expects a 0-based index.
    if( this.binarytree.size() > 0 )
      this.downHeap( 0 );

    return topnode;
  }

  /**
   * Sets the priority of a specific node in the heap, thereby also forcing
   * to fixup the heap to satisfy the heap condition.
   *
   * @param node The HeapNode object whose priority is to be changed
   * @param priority The new priority that will be assigned to the heapnode.
   */
  public void setPriority( HeapNode node, double priority ) {
    int index;

    // Check that it is a node in the heap
    if( this.binarytree.contains( node )) {
      // Change the priority of the node
      node.setPriority( priority );

      // Get the index of the node
      index = this.binarytree.indexOf( node );
      // Fixup the heap
      index = this.upHeap( index );
      this.downHeap( index );
    }
  }

  /**
   * Clears the heap, removing all nodes in the heap.
   */
  public void clear() {
    this.binarytree.clear();
  }

  /**
   * Checks if the heap is empty
   */
  public boolean isEmpty() {
    return this.binarytree.isEmpty();
  }

  /**
   * Determines if the given object is encapsulated by one of the nodes
   * in the heap. If it is, then the HeapNode encapsulating the object
   * is returned.
   */
  public HeapNode contains( Object object, Comparator heapnodeobjectcomparator ) {
    return (HeapNode) salvo.jesus.util.Collections.contains( this.binarytree, object, heapnodeobjectcomparator );
  }

  /**
   * Move the node, specified by the index, up in the heap until
   * heap condition is satisfied.
   *
   * @param  index  0-based index of the node we want to move up the heap.
   * @return  The new index of the node that we have moved up the heap.
   */
  private int upHeap( int index ) {
    HeapNode  current = (HeapNode) this.binarytree.get( index );
    HeapNode  parent;
    int       parentindex;

    do {
      // Do nothing if we are elready at the top of the heap
      if( index == 0 ) break;

      // Get the index of the parent object
      parentindex = index / 2;
      // Because List is 0-based, and even numbered index will have its
      // parent as index / 2 - 1;
      if( index % 2 == 0 )  parentindex--;
      parent = (HeapNode) this.binarytree.get( parentindex );

      // Compare the priorities of the current node against its parent.
      // If the current node has a higher priority, then
      // put the parent down to the position of the current node
      if( this.comparator.compare( current, parent ) > 0 )
        this.binarytree.set( index, parent );
      else
        // Heap condition satisfied
        break;

      // Move the index up
      index = parentindex ;
    } while( index > 0 );

    // Value of index should now be <= the value of index when this method
    // was called.
    this.binarytree.set( index, current );

    return index;
  }

  /**
   * Move the node, specified by the index, down in the heap until
   * the heap condition is satisfied.
   *
   * @param  index  0-based index of the node we want to move down the heap.
   * @return  The new index of the node that we have moved down the heap.
   */
  private int downHeap( int index ) {
    HeapNode  current = (HeapNode) this.binarytree.get( index );
    HeapNode  leftchild, rightchild, higherprioritychild;
    int       leftchildindex, rightchildindex, higherprioritychildindex;
    int       treesize = this.binarytree.size();

    do {
      // If heap is empty or has only one, then exit
      if( treesize <= 1 )
        break;

      leftchildindex = index * 2 + 1;
      rightchildindex = index * 2 + 2;

      // Get the left child of the current node
      if( leftchildindex >= treesize )
        leftchild = null;
      else
        leftchild = (HeapNode) this.binarytree.get( leftchildindex );

      // Get the right child of the current node
      if ( rightchildindex >= treesize )
        rightchild = null;
      else
        rightchild = (HeapNode) this.binarytree.get( rightchildindex );

      // If current node has no childen, exit loop
      if( leftchild == null && rightchild == null )
        break;
      // If current node has only one child (the left child), get that
      // as the child with the highest priority
      else if( leftchild != null && rightchild == null )
        higherprioritychildindex = leftchildindex;
      // If current node has both a left and right child node,
      // determine which of the two child nodes have higher priority
      else if( this.comparator.compare( leftchild, rightchild ) < 0 )
        higherprioritychildindex = rightchildindex;
      else
        higherprioritychildindex = leftchildindex;

      // So get the child node that has higher priority.
      higherprioritychild = (HeapNode) this.binarytree.get( higherprioritychildindex );

      // Compare the priority of the current node with the priority
      // of the child node that has higher priority.
      if( this.comparator.compare( current, higherprioritychild ) < 0 )
        // The child with the higher priority has a priority higher than
        // the current node. So place the child node up in the heap.
        this.binarytree.set( index, higherprioritychild );
      else
        // Heap condition satisfied
        break;

      // Move index down
      index = higherprioritychildindex;

    }while( index < treesize - 1 );

    // The final position of the node down the heap.
    this.binarytree.set( index, current );

    return index;
  }

  /**
   * Returns a String representation of the Heap.
   */
  public String toString() {
    return this.binarytree.toString();
  }

}