/*
 *   LinkedQueue.java
 *
 * Copyright 2000-2001-2002  aliCE team at deis.unibo.it
 *
 * This software is the proprietary information of deis.unibo.it
 * Use is subject to license terms.
 *
 */
package alice.util;
import java.io.Serializable;

/**
 *
 * classic queue managed with head and tail references
 *
 */
public class LinkedQueue implements Serializable {

    /** queue head  */
    public LinkedList head;

    /** queue tail */
    public LinkedList tail;

    /** build an empty queue */
    public LinkedQueue() {
        head = tail = new LinkedList();
    }

    /** head insert */
    public void insFirst(Object o) {
        head = new LinkedList(o,head);
    }

    /** tail instert */
    public void insLast(Object o) {
        tail.append(o);
        tail = tail.tail;
    }

    /** remove the first element */
    public Object remFirst() {
        Object o = head.head;
        head = head.tail;
        return(o);
    }

    public int length() {
        return(head.length());
    }

    public boolean isEmptyQueue() {
        return(head == tail);
    }

    public java.util.List toList(){
        java.util.List list=new java.util.LinkedList();
        alice.util.LinkedList l = head;
        while(l!=tail) {
           list.add(l.head);
           l = l.tail;
        }
        return list;
    }

}
