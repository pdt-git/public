/*
 */
package org.cs3.pl.prolog.internal.rpc;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import jpl.Query;
import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologSession;

/**
 * there was a problem with the predicate_property predicate. this test tries to
 * reproduce it.
 */
public class PredicatePropertyTest extends TestCase {
    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        ;
    }

    public synchronized void testIt() throws Throwable {
        RPCPrologInterface pif = new RPCPrologInterface();
        //pif.setStandAloneServer(true);
        pif.setPort(4143);
        //pif.setStartStrategy(new InProcessStartStrategy());
        pif.start();
        PrologSession session = pif.getSession();
      //  for (int i = 0; i < 100; i++) {
            Map solution = session
                    .query("predicate_property(P,built_in),functor(P,Name,_)");
            List keywords = new ArrayList();
            while (solution != null) {
                //

                String name = (String) solution.get("Name");
                Debug.debug(name);
                keywords.add(name);
                if (name.equals("goal_expansion")) {
                    Debug.debug("bum.");
                }
                solution = session.next();
            }
        //}
    }

    public synchronized void __testNochWas() throws Throwable {

        Query q = new Query("predicate_property(P,built_in),functor(P,Name,_)");

        List keywords = new ArrayList();
        while (q.hasMoreSolutions()) {
            Hashtable solution = q.nextSolution();

            String name = solution.get("Name").toString();
            Debug.debug(name);
            keywords.add(name);
            if (name.equals("goal_expansion")) {
                Debug.debug("bum.");
            }

        }
    }
}
