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


/** Mutex-lock for synchronization in multithreaded enviroments.*/
public class Mutex  {
    boolean locked = false;
    boolean debugging = false;
    String name;
    
    public Mutex() {
        this.name = "";
    }
    
    public Mutex(String name) {
        this.name = name;
    }
    
    /** tries to aquire a lock. If the lock is hold by another thread, the current thread will
     * wait until the lock is released.   
     *  */
    final public synchronized void aquire() throws InterruptedException {
        aquire(0);
    }

    /** Releases a lock. All threads that are waiting for the lock gets notified, but only one can
     * continue executing the aquire method (because its synchronized) and get the new lock.*/
    final public synchronized void release() {
        log("Lock released");
        locked = false;
        notifyAll();
    }

    
    /** Tries to aquire a lock for the specified time and returns false, if the lock can't be aquired in that time.*/
    final public synchronized boolean aquire(int time) throws InterruptedException {
        log("Try to aquire lock");
        
        if ( time <= 0 ) { 
            while (locked) {
                wait();
            }
        } else {
             long startTime = System.currentTimeMillis();
             long currentTime = startTime; 
        
             //The synchronization monitor will be released during wait
             while (locked) {
                 long diff = (currentTime - startTime);
                 if ( diff >= time) {
                     log("Timeout when waiting for the log");
                     return false;
                 }
                 wait( time - diff);
                 currentTime = System.currentTimeMillis();
             }
        }
        locked = true; //Only one thread can execute the aquire method at one time
        log("Lock aquired");
        return true;
    }
    
    String getLoggingPrefix() {
        return "Mutex " + name + " " + Thread.currentThread().toString()  + " ";
    }
    
    void log(String message) {
        if ( debugging )
            System.out.println( getLoggingPrefix() + message);
    }
}




