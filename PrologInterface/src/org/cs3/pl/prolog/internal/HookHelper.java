/*
 */
package org.cs3.pl.prolog.internal;

import java.util.HashMap;
import java.util.Iterator;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologSession;


/**
 */
public class HookHelper {
    private final class StartupThread extends Thread {

        private StartupThread(String name) {
            super(name);
        }

        public void run() {
            synchronized (hooks) {
                /*
                 * why clone AND sync?
                 * ld: sync to make sure that hooks operation is serialized 
                 * clone to make it possible to (un)register hooks in the hook 
                 * methods - this would not be possible otherwise since the 
                 * iterator would throw a ConcurrentModificationException.
                 */
                HashMap cloned = (HashMap) hooks.clone();
                hookFilpFlop = !hookFilpFlop;
                for (Iterator it = cloned.keySet().iterator(); it.hasNext();) {
                    LifeCycleHookWrapper h = (LifeCycleHookWrapper) cloned
                            .get(it.next());
                    if (h.flipflop != hookFilpFlop) {
                        h.afterInit();
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

    public void addLifeCycleHook(LifeCycleHook h) {
        addLifeCycleHook(h, null, null);
    }

    /**
     * @param hook
     * @param id
     * @param dependsOn
     */
    public void addLifeCycleHook(LifeCycleHook hook, String id,
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

    public void onInit(PrologSession initSession) {
        HashMap cloned=null;
        synchronized (hooks) {
            cloned = (HashMap) hooks.clone();     
            /*
             * why clone AND sync?
             * ld: sync to make sure that hooks operation is serialized 
             * clone to make it possible to (un)register hooks in the hook 
             * methods - this would not be possible otherwise since the 
             * iterator would throw a ConcurrentModificationException.
             */
            
            hookFilpFlop = !hookFilpFlop;
            for (Iterator it = cloned.keySet().iterator(); it.hasNext();) {
                LifeCycleHookWrapper h = (LifeCycleHookWrapper) cloned.get(it
                        .next());
                if (h.flipflop != hookFilpFlop) {
                    h.onInit(initSession);
                }
            }
        }
    }

    public void afterInit() {
        startupThread = new StartupThread("PrologInterface startup Thread");
        startupThread.start();
    }
    public void beforeShutdown(PrologSession s){
        
            //ld:gotta get in to get out
            if (startupThread != null) {
                try {
                    startupThread.join();
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
            
            if (s != null) {
                synchronized (hooks) {
                    /*
                     * why clone AND sync?
                     * ld: sync to make sure that hooks operation is serialized 
                     * clone to make it possible to (un)register hooks in the hook 
                     * methods - this would not be possible otherwise since the 
                     * iterator would throw a ConcurrentModificationException.
                     */
                    
                    HashMap cloned = (HashMap) hooks.clone();
                    hookFilpFlop = !hookFilpFlop;
                    for (Iterator it = cloned.keySet().iterator(); it.hasNext();) {
                        String id = (String) it.next();
                        LifeCycleHookWrapper h = (LifeCycleHookWrapper) cloned
                                .get(id);
                        if (h.flipflop != hookFilpFlop) {
                            try {
                                h.beforeShutdown(s);
                            } catch (Throwable t) {
                                Debug
                                        .error("could not execute 'beforeShutdown' on hook '"
                                                + id + "'");
                                Debug.report(t);
                            }
                        }
                    }
                }

                try {
                    s.dispose();
                } catch (Throwable t) {
                    Debug.error("could not dispose shutdown session.");
                    Debug.report(t);
                }
            }
        }
}
