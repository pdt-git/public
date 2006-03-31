/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pl.prolog.internal.socket.observable;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Set;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceListener;

/**
 */
public class PrologEventDispatcher {
    SocketClient client;

    Thread dispatchThread;

    protected boolean shouldBeRunning;

    private LinkedList queue = new LinkedList();

    private HashMap listenerLists = new HashMap();

    private Object source;

    public PrologEventDispatcher(Object source)
            throws IOException {
        
        this.source = source;
    }

    private void enableSubject(final String subject) {
        if(!shouldBeRunning){
            return;
        }
        if (Thread.currentThread() != dispatchThread) {
            synchronized (queue) {
                queue.addLast(new Runnable() {
                    public void run() {
                        enableSubject(subject);
                    }
                });
            }
        } else {
            client.lock();
            try {

                client.writeln("");
                client.readUntil(SocketClient.GIVE_COMMAND);
                client.writeln(SocketClient.REGISTER_OBSERVER);
                client.readUntil(SocketClient.GIVE_SUBJECT);
                client.writeln(subject);

            } catch (Exception e) {
                Debug.report(e);
                throw new RuntimeException(e);
            } finally {
                client.unlock();
            }
        }
    }

    private void disableSubject(final String subject) {
        if(!shouldBeRunning){
            return;
        }
        
        if (Thread.currentThread() != dispatchThread) {
            synchronized (queue) {
                queue.addLast(new Runnable() {
                    public void run() {
                        disableSubject(subject);
                    }
                });
            }
        } else {
            client.lock();
            try {

                client.writeln("");
                client.readUntil(SocketClient.GIVE_COMMAND);
                client.writeln(SocketClient.UNREGISTER_OBSERVER);
                client.readUntil(SocketClient.GIVE_SUBJECT);
                client.writeln(subject);

            } catch (Exception e) {
                Debug.report(e);
                throw new RuntimeException(e);
            } finally {
                client.unlock();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Thread#start()
     */
    public synchronized void start(SocketClient sclient) {
    	if(sclient==null){
    		throw new IllegalArgumentException("null is not a valid client!");
    	}
        
        if (dispatchThread != null) {
        	Debug.warning("erm... i'm already running. Refusing to start another time");
        	return;
        }
        
        this.client=sclient;
        
        
        synchronized (queue) {
            queue.clear();
        }
        dispatchThread = new Thread("Prolog Event Dispatcher") {
            public void run() {
                client.lock();
                try {
                    synchronized(listenerLists){
                        Set set = listenerLists.keySet();
                        for (Iterator it = set.iterator(); it.hasNext();) {
                            String subject = (String) it.next();
                            enableSubject(subject);
                        }
                    }
                    while (shouldBeRunning) {
                        dispatch();
                    }
                    synchronized(listenerLists){
                        Set set = listenerLists.keySet();
                        for (Iterator it = set.iterator(); it.hasNext();) {
                            String subject = (String) it.next();
                            disableSubject(subject);
                        }
                    }
                    client.writeln("");
                } catch (Exception e) {
                    Debug.report(e);
                    throw new RuntimeException(e);
                } finally {
                    client.unlock();
                }
            }
        };
        shouldBeRunning = true;
        dispatchThread.start();
    }

    public synchronized void stop() {
        if (dispatchThread != null) {
            shouldBeRunning = false;
            try {
                dispatchThread.join();
                client.close();
                client=null;
				dispatchThread=null;
            } catch (InterruptedException e) {
                Debug.report(e);
                throw new RuntimeException(e);
            } catch (IOException e) {
                Debug.report(e);
                throw new RuntimeException(e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Thread#run()
     */
    protected void dispatch() throws PrologException, IOException,
            InterruptedException {
        sleep();
        synchronized (queue) {
            if (!queue.isEmpty()) {
                while (!queue.isEmpty()) {
                    Runnable r = (Runnable) queue.removeFirst();
                    r.run();
                }
                client.readUntil(SocketClient.GIVE_COMMAND);
                client.writeln(SocketClient.LISTEN);
            }
        }
        BufferedReader reader = client.getReader();
        client.skipWhitespace(false);
        while (reader.ready()) {
            String varname = (String) client.readValue();
            if (varname == null) {
                //there was no respective data
                String line = client.readln();
                //Debug.debug("parsing: "+line);
                if (line == null) {
                    throw new PrologException("don't know what to do.");
                }
                if (line.startsWith(SocketClient.ERROR)) {
                    throw new PrologException("Peer reported an error:"
                            + line.substring(SocketClient.ERROR.length()));
                }

                if (SocketClient.NO.equals(line)) {//no

                    return;
                }
                if (SocketClient.OK.equals(line)) {//no

                    return;
                }
            } else {
                Object value = client.readValue();
                if (value == null) {
                    throw new PrologException(
                            "could not read value for variable " + varname);
                }
                fireUpdate(varname, value.toString());
            }
            client.skipWhitespace(false);
        }
    }

    /**
     * @throws IOException
     * @throws InterruptedException
     *  
     */
    private void sleep() throws IOException, InterruptedException {
        boolean sleepOn = shouldBeRunning;
        while (sleepOn) {
        	sleepOn&=shouldBeRunning;
            synchronized (queue) {
                sleepOn &= queue.isEmpty();
            }
            sleepOn &= !client.getReader().ready();
            if (sleepOn) {
                Thread.sleep(100);
            }
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IPrologInterface#addPrologInterfaceListener(java.lang.String,
     *         org.cs3.pl.prolog.PrologInterfaceListener)
     */
    public void addPrologInterfaceListener(String subject,
            PrologInterfaceListener l) {
        synchronized (listenerLists) {
            Vector list = (Vector) listenerLists.get(subject);
            if (list == null) {
                list = new Vector();
                listenerLists.put(subject, list);
                enableSubject(subject);
            }
            if (!list.contains(l)) {
                list.add(l);
            }
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IPrologInterface#removePrologInterfaceListener(java.lang.String,
     *         org.cs3.pl.prolog.PrologInterfaceListener)
     */
    public void removePrologInterfaceListener(String subject,
            PrologInterfaceListener l) {
        synchronized (listenerLists = new HashMap()) {
            Vector list = (Vector) listenerLists.get(subject);
            if (list == null) {
                return;
            }
            if (list.contains(l)) {
                list.remove(l);                
            }
            if(list.isEmpty()){
                disableSubject(subject);
                listenerLists.remove(subject);
            }

        }

    }

    /**
     * @param subject2
     * @param string
     */
    private void fireUpdate(String subject, String event) {
        Vector listeners = (Vector) listenerLists.get(subject);
        if (listeners == null) {
            return;
        }
        PrologInterfaceEvent e = new PrologInterfaceEvent(source, subject,
                event);

        Vector cloned = null;
        synchronized (listeners) {
            cloned = (Vector) listeners.clone();
        }
        for (Iterator it = cloned.iterator(); it.hasNext();) {
            PrologInterfaceListener l = (PrologInterfaceListener) it.next();
            l.update(e);
        }
    }
}
