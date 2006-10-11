/*
 *   LinkedList.java
 *
 * Copyright 2000-2001-2002  aliCE team at deis.unibo.it
 *
 * This software is the proprietary information of deis.unibo.it
 * Use is subject to license terms.
 *
 */
package alice.util;
import	java.io.*;

/**
 *
 * prolog/lisp style list, with in evidence head and tail
 *
 */
public class LinkedList implements Serializable {
    public Object head;
    public LinkedList tail;

    /** empty list */
    public LinkedList() {
        head = null;
        tail = null;
    }

    /** list from head and tail */
    public LinkedList(Object h,LinkedList t) {
        head = h;
        tail = t;
    }

    /** head insert */
    public void insert(Object h) {
        LinkedList t = new LinkedList(head,tail);
        head = h;
        tail = t;
    }

    /** tail insert */
    public void append(Object h) {
        LinkedList l = this;
        while(l.tail != null)
            l = l.tail;
        l.head = h;
        l.tail = new LinkedList();
    }

    public void delete(Object h) {
        LinkedList l = this;
        while(l.tail != null) {
            if(l.head == h) {
                l.head = l.tail.head;
                l.tail = l.tail.tail;
                return;
            }
            l = l.tail;
        }
    }

    public int length() {
        int c = 0;
        LinkedList l = this;
        while(l.tail != null) {
            c++;
            l = l.tail;
        }
        return(c);
    }

    public boolean isEmptyList() {
        return(tail == null);
    }

    public Object getHead(){
        return head;
    }

    public LinkedList getTail(){
        return tail;
    }
    
    public java.util.List toList(){
        java.util.List list=new java.util.LinkedList();
        alice.util.LinkedList l = this;
        while(!l.isEmptyList()) {
           list.add(l.head);
           l = l.tail;
        }
        return list;
    }
}
