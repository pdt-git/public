/*--------------------------------------------------------------------------*
  | Copyright (C) 2001 Christopher Kohlhaas                                  |
  |                                                                          |
  | This program is free software; you can redistribute it and/or modify     |
  | it under the terms of the GNU General Public License as published by the |
  | Free Software Foundation. A copy of the license has been included with   |
  | these distribution in the COPYING file, if not go to www.fsf.org         |
  |                                                                          |
  | As a special exception, you are granted the permissions to link this     |
  | program with every library, which license fulfills the Open Source       |
  | Definition as published by the Open Source Initiative (OSI).             |
  *--------------------------------------------------------------------------*/
package org.rapla.components.rpc;

import java.util.LinkedList;
public class Queue {
    int maxSize = -1;
    LinkedList list = new LinkedList();

    public Queue() {
    }
    
    public Queue(int maxSize) {
        this.maxSize = maxSize;
    }

    public boolean isFull() {
        return (maxSize>0 && list.size()>= maxSize);
    }

    public boolean isEmpty() {
        return list.size() == 0;
    }
    
    /** @throws QueueFullException if queue is full */;
    public synchronized void enqueue(final Object object) throws QueueFullException {
    	if (isFull())
    	    throw new QueueFullException();
    	list.addLast(object);
    }

    /** @throws NoSuchElementException if empty */;
    public synchronized Object dequeue() {
        return list.removeFirst();
    }

}

