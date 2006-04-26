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
package org.cs3.pl.prolog.internal;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class HookHelper {
    PrologInterface pif;
    /**
     * @param pif
     */
    public HookHelper(PrologInterface pif) {
        super();
        this.pif = pif;
    }
    private final class StartupThread extends Thread {

        private StartupThread(String name) {
            super(name);
        }

        public void run() {
            HashMap cloned = null;
            synchronized (hooks) {
                /*
                 * why clone AND sync? ld: sync to make sure that hooks
                 * operation is serialized clone to make it possible to
                 * (un)register hooks in the hook methods (which do run on the
                 * same thread and are not blocked by the synchronized) - this
                 * would not be possible otherwise since the iterator would
                 * throw a ConcurrentModificationException.
                 */
                batchUpdateHooks();
                cloned = (HashMap) hooks.clone();

                hookFilpFlop = !hookFilpFlop;
                for (Iterator it = cloned.keySet().iterator(); it.hasNext();) {
                    LifeCycleHookWrapper h = (LifeCycleHookWrapper) cloned
                            .get(it.next());
                    if (h.flipflop != hookFilpFlop) {
                        h.afterInit(pif);
                    }
                }
            }
        }

        public void start() {
            setDaemon(true);
            super.start();
        }
    }

    private StartupThread startupThread;

    private boolean hookFilpFlop = false;

    private HashMap hooks = new HashMap();

    private Vector queue = new Vector();

    private void batchUpdateHooks() {
        synchronized (queue) {
            for (Iterator it = queue.iterator(); it.hasNext();) {
                Runnable r = (Runnable) it.next();
                r.run();
            }
            queue.clear();
        }
    }

    public void addLifeCycleHook(final LifeCycleHook hook, final String id,
            final String[] dependencies) {
        synchronized (queue) {
            queue.add(new Runnable() {
                public void run() {
                    addLifeCycleHook_impl(hook,id,dependencies);
                }
            });
        }
    }

    /**
     * @param hook
     * @param id
     * @param dependsOn
     */
    private void addLifeCycleHook_impl(LifeCycleHook hook, String id,
            String[] dependencies) {
        synchronized (hooks) {
            if (id == null) {
                id = "<<" + hooks.size() + ">>";
            }
            if (dependencies == null) {
                dependencies = new String[0];
            }
            Debug.debug("requested to add hook: id=\"" + id
                    + "\", dependencies=\"" + Util.prettyPrint(dependencies)
                    + "\"");

            LifeCycleHookWrapper node = (LifeCycleHookWrapper) hooks.get(id);

            if (node == null) {
                Debug.debug("\t-> hook unknown, new wrapper created.");
                node = new LifeCycleHookWrapper(hook, id);
                node.flipflop = hookFilpFlop;
                hooks.put(id, node);
            } else {
                Debug
                        .debug("\t-> hook exists, reusing wrapper, but replacing hook code..");
                node.hook = hook;
            }
            for (int i = 0; i < dependencies.length; i++) {
                LifeCycleHookWrapper dep = (LifeCycleHookWrapper) hooks
                        .get(dependencies[i]);
                Debug.debug("\t-> looking up dependency \"" + dependencies[i]
                        + "\"");
                if (dep == null) {
                    Debug.debug("\t\t-> hook unknown, new wrapper created.");
                    dep = new LifeCycleHookWrapper(null, dependencies[i]);
                    dep.flipflop = hookFilpFlop;
                    hooks.put(dependencies[i], dep);
                }
                dep.pre.add(node);
                node.post.add(dep);
                Debug.debug("\t-> edges added.");
            }
        }

    }
    public void removeLifeCycleHook(final String hookId) {
        synchronized (queue) {
            queue.add(new Runnable() {
                public void run() {
                    removeLifeCycleHook_impl(hookId);
                }
            });
        }
    }
    private void removeLifeCycleHook_impl(String hookId) {
        synchronized (hooks) {
            LifeCycleHookWrapper h = (LifeCycleHookWrapper) hooks.get(hookId);
            if (h == null) {
                return;
            }
            hooks.remove(h);
            for (Iterator it = h.pre.iterator(); it.hasNext();) {
                LifeCycleHookWrapper elm = (LifeCycleHookWrapper) it.next();
                elm.post.remove(h);
            }
            for (Iterator it = h.post.iterator(); it.hasNext();) {
                LifeCycleHookWrapper elm = (LifeCycleHookWrapper) it.next();
                elm.pre.remove(h);
            }
            h.hook = null;//extra paranoia :-)
        }
    }

    public void onInit(PrologSession initSession) {
        HashMap cloned = null;
        synchronized (hooks) {
            batchUpdateHooks();
            cloned = (HashMap) hooks.clone();
            /*
             * why clone AND sync? ld: sync to make sure that hooks operation is
             * serialized clone to make it possible to (un)register hooks in the
             * hook methods (which do run on the same thread and are not blocked
             * by the synchronized) - this would not be possible otherwise since
             * the iterator would throw a ConcurrentModificationException.
             */

            hookFilpFlop = !hookFilpFlop;
            for (Iterator it = cloned.keySet().iterator(); it.hasNext();) {
                LifeCycleHookWrapper h = (LifeCycleHookWrapper) cloned.get(it
                        .next());
                if (h.flipflop != hookFilpFlop) {
                    h.onInit(pif,initSession);
                }
            }
        }
    }

    public void afterInit() {
        startupThread = new StartupThread("PrologInterface startup Thread");
        startupThread.start();
    }

    public void beforeShutdown(PrologSession s) {

        //ld:gotta get in to get out
        if (startupThread != null) {
            try {
                startupThread.join();
            } catch (InterruptedException e) {
                Debug.rethrow(e);
            }
        }

        if (s != null) {
            HashMap cloned = null;
            synchronized (hooks) {
                batchUpdateHooks();
                cloned = (HashMap) hooks.clone();

                hookFilpFlop = !hookFilpFlop;
                for (Iterator it = cloned.keySet().iterator(); it.hasNext();) {
                    String id = (String) it.next();
                    LifeCycleHookWrapper h = (LifeCycleHookWrapper) cloned
                            .get(id);
                    if (h.flipflop != hookFilpFlop) {
                        try {
                            h.beforeShutdown(pif,s);
                        } catch (Throwable t) {
                            Debug
                                    .error("could not execute 'beforeShutdown' on hook '"
                                            + id + "'");
                            Debug.report(t);
                        }
                    }
                }
            }
            
        }
    }
}
