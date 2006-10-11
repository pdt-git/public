/*                          ge
 * tuProlog - Copyright (C) 2001-2002  aliCE team at deis.unibo.it
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package alice.tuprolog;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import alice.util.LinkedList;

/**
 *
 * The Prolog class represents a tuProlog engine.
 *
 */
public class Prolog implements Serializable {

    // 2P version
    private static final String VERSION = "1.3.0 ";

    /*
     * possible values returned by step functions
     * and used as eval state flags
     */
    /* eval just started */
    private static final int INIT = 0;
    /* false goal */
    private static final int FALSE = 1;
    /* ask for continuing eval cycle */
    private static final int CONT = 2;
    /* true goal */
    private static final int TRUE = 3;
    /* true goal with open alternatives */
    private static final int TRUE_CP = 4;
    /* the demo process must halt */
    private static final int HALT = 5;

    /*
     *  some of the core builtin predicates
     *  arranged as matrix for fast recognition
     *  during identifying process
     */
    private static final String[][] BUILTIN =
            {
                {"fail", "true", "halt", "!", "$restore_db"},
                {"$asserta", "$assertz", "$retract", "halt", "load_library", "unload_library", "flag_list"},
                {",", "is", "=", "\\=", "$tolist", "$fromlist", "$copy", "$append", "$find", "flag"},
                {"op"}
            };
    
    
    /* runtime context of the demonstration */
    private RTContext currentContext;
    /* the process demo must stop? */
    private boolean mustStop;
    /*  manager of current theory */
    private TheoryManager theoryManager;
    /* component managing operators inside the engine */
    private OperatorManager opManager;    
    /* flag list */
    private ArrayList flags;
    /* current goal to be evaluated */
    private Term currentGoal;
    /* start query */
    private Term startQuery;
    /* vars present in current goal */
    private alice.util.LinkedList currentVars;
    /* eval result (FALSE,TRUE,TRUE_CP,...) */
    private int evalFlag;
    /* in the case of HALT flag */
    private int haltCode;

    /* dynamically loaded built-in libraries */
    private ArrayList currentLibraries;
    /* builtin predicates currently defined by libraries */
    private HashMap extPredicates;    
    /* builtin evaluable functors currently defined by libraries */
    private HashMap extFunctors;
    
    /*  spying activated ?  */
    private boolean spy;  
    /*  warning activated ?  */
    private boolean warning;
    /* listeners registrated for virtual machine output events */
    private ArrayList outputListeners;
    /* listeners registrated for virtual machine internal events */
    private ArrayList spyListeners;
    /* listeners registrated for virtual machine state change events */
    private ArrayList warningListeners;
    
    /* listeners to theory events */
    private ArrayList theoryListeners;
    /* listeners to library events */
    private ArrayList libraryListeners;
    /* listeners to query events */
    private ArrayList queryListeners;
    

    /**
     * Builds a prolog engine with default libraries loaded.
     *
     * The default libraries are BasicLibrary, ISOLibrary,
     * IOLibrary, and  JavaLibrary
     */
    public Prolog() {
        this(false,true);
        try {
            loadLibrary("alice.tuprolog.lib.BasicLibrary");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        try {
            loadLibrary("alice.tuprolog.lib.ISOLibrary");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        try {
            loadLibrary("alice.tuprolog.lib.IOLibrary");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        try {
            loadLibrary("alice.tuprolog.lib.JavaLibrary");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Builds a tuProlog engine with loaded
     * the specified libraries
     *
     * @param libs the (class) name of the libraries to be loaded
     */
    public Prolog(String[] libs) throws InvalidLibraryException {
        this(false,true);
        if (libs != null) {
            for (int i = 0; i < libs.length; i++) {
                loadLibrary(libs[i]);
            }
        }
    }

    /**
     * Initialize basic engine structures.
     * 
     * @param spy spying activated
     * @param warning warning activated
     */
    private Prolog(boolean spy, boolean warning){
        currentLibraries = new ArrayList();
        extPredicates = new HashMap();
        extFunctors = new HashMap();
        outputListeners = new ArrayList();
        spyListeners = new ArrayList();
        warningListeners = new ArrayList();
        this.spy = spy;
        this.warning = warning;
        opManager = new OperatorManager();
        flags = new ArrayList();
        theoryListeners = new ArrayList();
        queryListeners = new ArrayList();
        libraryListeners = new ArrayList();
        theoryManager = new TheoryManager(this);
    }
    
    /**
     * Gets the current version of the tuProlog system
     */
    public static String getVersion() {
        return VERSION;
    }

    // theory management interface

    /**
     * Sets a new theory
     *
     * @param th is the new theory
     * @throws InvalidTheoryException if the new theory is not valid
     * @see Theory
     */
    public synchronized void setTheory(Theory th) throws InvalidTheoryException {
        Theory oldTh = theoryManager.getLastConsultedTheory();
        theoryManager.consult(th, false, true, null);
        if (theoryManager.getStartGoal() != null) {
            try {
                Term bak_goal = currentGoal;
                alice.util.LinkedList bak_vars = currentVars;
                solve(theoryManager.getStartGoal());
                currentGoal = bak_goal;
                currentVars = bak_vars;
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
        Theory newTh = theoryManager.getLastConsultedTheory();
        TheoryEvent ev = new TheoryEvent(this,oldTh,newTh);	
        this.notifyChangedTheory(ev);
    }

    /**
     * Adds (appends) a theory
     *
     * @param th is the theory to be added
     * @throws InvalidTheoryException if the new theory is not valid
     * @see Theory
     */
    public synchronized void addTheory(Theory th) throws InvalidTheoryException {
        Theory oldTh = theoryManager.getLastConsultedTheory();
        theoryManager.consult(th, true, true, null);
        if (theoryManager.getStartGoal() != null) {
            try {
                Term bak_goal = currentGoal;
                alice.util.LinkedList bak_vars = currentVars;
                solve(theoryManager.getStartGoal());
                currentGoal = bak_goal;
                currentVars = bak_vars;
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
        Theory newTh = theoryManager.getLastConsultedTheory();
        TheoryEvent ev = new TheoryEvent(this,oldTh,newTh);	
        this.notifyChangedTheory(ev);
    }

    /**
     * Gets current theory
     *
     * @return current(dynamic) theory
     */
    public synchronized Theory getTheory() {
        try {
            return new Theory(theoryManager.getTheory(true));
        } catch (Exception ex){
            return null;
        }
    }

    /**
     * Gets last consulted theory, with the original textual format
     *  
     * @return theory
     */
    public synchronized Theory getLastConsultedTheory() {
        try {
            return theoryManager.getLastConsultedTheory();
        } catch (Exception ex){
            return null;
        }
    }

    
    
    /**
     * Clears current theory
     */
    public synchronized void clearTheory() {
        Theory oldTh = theoryManager.getLastConsultedTheory();
        theoryManager.clear(true);
        Theory newTh = theoryManager.getLastConsultedTheory();
        TheoryEvent ev = new TheoryEvent(this,oldTh,newTh);	
        this.notifyChangedTheory(ev);
    }


    // libraries management interface

    /**
     * Loads a library.
     *
     * If a library with the same name is already present,
     * a warning event is notified and the request is ignored.
     *
     * @param className name of the Java class containing the library to be loaded
     * @return the reference to the Library just loaded
     * @throws InvalidLibraryException if name is not a valid library
     */
    public synchronized Library loadLibrary(String className) throws InvalidLibraryException {
        Library lib=null;
        try {
			lib = (Library) Class.forName(className).newInstance();
			String name=lib.getName();
            boolean found = false;
            Library alib = null;
            Iterator it = currentLibraries.listIterator();
            while (it.hasNext()){
                alib = (Library) it.next();
                if (alib.getName().equals(name)) {
                    found = true;
                    break;
                }
            }
            if (found) {
                if (warning){
                    notifyWarning(new WarningEvent(this,"library "+alib.getName()+
                            " already loaded."));
                }
                return alib;
            }
        } catch (Exception ex){
            throw new InvalidLibraryException(className,-1,-1);
        }
        bindLibrary(lib);
        LibraryEvent ev = new LibraryEvent(this,lib.getName());
        notifyLoadedLibrary(ev);
        return lib;
    }

    /**
     * Loads a specific instance of a library
     *
     * If a library with the same name is already present,
     * a warning event is notified 
     * 
     * @param lib the (Java class) name of the library to be loaded
     * @throws InvalidLibraryException if name is not a valid library
     */
    public synchronized void loadLibrary(Library lib) throws InvalidLibraryException {
        boolean found = false;
        String name = lib.getName();
        Library alib = null;
        Iterator it = currentLibraries.listIterator();
        while (it.hasNext()){
            alib = (Library) it.next();
            if (alib.getName().equals(name)) {
                found = true;
                break;
            }
        }
        if (found) {
            if (warning){
                notifyWarning(new WarningEvent(this,"library "+alib.getName()+
                    " already loaded."));
            }
            unloadLibrary(name);
        }
        bindLibrary(lib);
        LibraryEvent ev = new LibraryEvent(this,lib.getName());
        notifyLoadedLibrary(ev);
    }

    /**
     * Gets the list of current libraries loaded
     *
     * @return the list of the library names
     */
    public synchronized String[] getCurrentLibraries() {
        String[] libs = new String[currentLibraries.size()];
        for (int i=0; i<libs.length; i++){
            libs[i]=((Library)currentLibraries.get(i)).getName();
        }
        return libs;
    }
    /**
     * Unloads a previously loaded library
     *
     * @param name of the library to be unloaded
     * @throws InvalidLibraryException if name is not a valid loaded library
     */
    public synchronized void unloadLibrary(String name) throws InvalidLibraryException {
        boolean found = false;
        Iterator it0 = currentLibraries.listIterator();
        while (it0.hasNext()){
            Library lib = (Library) it0.next();
            if (lib.getName().equals(name)) {
                found = true;
                it0.remove();
                lib.dismiss();
                Iterator it = lib.getPredicates().iterator();
                while (it.hasNext()) {
                    extPredicates.remove(it.next());
                }
                it = lib.getFunctors().iterator();
                while (it.hasNext()) {
                    extFunctors.remove(it.next());
                }
                break;
            }
        }
        if (!found) {
            throw new InvalidLibraryException();
        }
        theoryManager.removeLibraryTheory(name);
        theoryManager.rebindBuiltins();
        LibraryEvent ev = new LibraryEvent(this,name);
        notifyUnloadedLibrary(ev);

    }


    /**
     * Binds a library.
     *
     * @param lib is library object
     * @return the reference to the Library just loaded
     * @throws InvalidLibraryException if name is not a valid library
     */
    private Library bindLibrary(Library lib) throws InvalidLibraryException {
        try {
            String name = lib.getName();
            lib.setEngine(this);
            currentLibraries.add(lib);
            Iterator it = lib.getPredicates().iterator();
            while (it.hasNext()) {
                String pred_name = (String)it.next();
                if (warning){
                    Library alib = (Library) extPredicates.get(pred_name);
                    if (alib!=null){
                        notifyWarning(new WarningEvent(this,"predicate "+pred_name+" already defined in library "+
                                alib.getName()));
                    }
                }
                extPredicates.put(pred_name, lib);
            }
            it = lib.getFunctors().iterator();
            while (it.hasNext()) {
                String fun_name = (String)it.next();
                Library alib = (Library) extFunctors.get(fun_name);
                if (warning){
                    if (alib!=null){
                        if (warning){
                            notifyWarning(new WarningEvent(this,"functor "+name+" already defined in library "+
                                alib.getName()));
                        }
                    }
                }
                extFunctors.put(fun_name, lib);
            }
            String th = lib.getTheory();
            if (th != null) {
                theoryManager.consult(new Theory(th), true, false, name);
                if (theoryManager.getStartGoal() != null) {
                    try {
                        Term bak_goal = currentGoal;
                        alice.util.LinkedList bak_vars = currentVars;
                        solve(theoryManager.getStartGoal());
                        currentGoal = bak_goal;
                        currentVars = bak_vars;
                    } catch (Exception ex) {
                    }
                }
            }
            // in current theory there could be predicates and functors
            // which become builtins after lib loading
            theoryManager.rebindBuiltins();
            //
            return lib;
        } catch (InvalidTheoryException ex) {
            //System.out.println("line "+ex.line+"  "+ex.pos);
            throw new InvalidLibraryException(lib.getName(),ex.line,ex.pos);
        } catch (Exception ex) {
            throw new InvalidLibraryException(lib.getName(),-1,-1);
        }
    }

    /**
     * Gets the reference to a loaded library
     *
     * @param name the (Java class) name of the library already loaded
     * @return the reference to the library loaded, null if the library is
     *         not found
     */
    public synchronized Library getLibrary(String name) {
        Iterator it = currentLibraries.listIterator();
        while (it.hasNext()){
            Library alib = (Library) it.next();
            if (alib.getName().equals(name)) {
                return alib;
            }
        }
        return null;
    }


    protected Library getLibraryPredicate(String name, int nArgs){
        return (Library)extPredicates.get(name + "_" + nArgs);
    }

    protected Library getLibraryFunctor(String name, int nArgs){
            return (Library)extFunctors.get(name + "_" + nArgs);
    }

    // operators management

    /**
     *  Gets the list of the operators currently defined
     *
     *  @return the list of the operators
     */
    public synchronized java.util.List getCurrentOperatorList() {
        return opManager.getOperators().toList();
    }

    /** gets the component managing operators inside the engine */
    OperatorManager getOperatorManager() {
        return opManager;
    }


    // solve interface

    /**
     *  Solves a query
     *
     * @param g the term representing the goal to be demonstrated
     * @return the result of the demonstration
     * @see SolveInfo
     **/
    public synchronized SolveInfo solve(Term g) {
        //System.out.println("ENGINE SOLVE #0: "+g);
        if (g == null) {
            return null;
        }
        try {
            startQuery = g.copy();
            
            g.resolveVariables(0);

            //System.out.println("solving "+g);
            Iterator it = currentLibraries.listIterator();
            while (it.hasNext()){
                ((Library) it.next()).onSolveBegin(g);
            }
            identify(g, false);
            
            /*
             * 20050910
             * The RTContext is initialised with a value
             * for variable timestamp renaming, to solve
             * the bagof bug.  
             */
            currentContext = new RTContext(1000000);
            
            mustStop = false;
            currentVars = new alice.util.LinkedList();
            currentGoal = g.copy(currentVars);
            evalFlag = INIT;
            //System.out.println("ENGINE SOLVE #1: "+currentGoal);
            theoryManager.transBegin();
            init(currentContext, currentGoal, 0, Var.rename(currentVars, 0, false), 0);
            haltCode = 0;
            evalFlag = eval();
			SolveInfo sinfo = null;
            if (evalFlag==TRUE||evalFlag==TRUE_CP){
            		sinfo = new SolveInfo(startQuery,toVarArray(currentVars), currentGoal.copy());
            } else {
            		sinfo = new SolveInfo(startQuery);
            }
              
            if (evalFlag == FALSE || evalFlag == TRUE || evalFlag == HALT) {
                solveEnd();
            }
            
            QueryEvent ev = new QueryEvent(this,sinfo);
            notifyNewQueryResultAvailable(ev);
            
            return sinfo;
        } catch (Exception ex) {
            ex.printStackTrace();
            return new SolveInfo(startQuery);
        }
    }

    /**
     * Solves a query
     *
     * @param st the string representing the goal to be demonstrated
     * @return the result of the demonstration
     * @see SolveInfo
     **/
    public synchronized SolveInfo solve(String st) throws MalformedGoalException {
        try {
            Term t = Term.parseSentence(st, opManager);
            return solve(t);
        } catch (InvalidTermException ex) {
            throw new MalformedGoalException();
        }
    }

    /**
     * Asks for the presence of open alternatives to be explored
     * in current demostration process.
     *
     * @return true if open alternatives are present
     */
    public synchronized boolean hasOpenAlternatives() {
        return evalFlag == Prolog.TRUE_CP;
    }

	/**
	 * Checks if the demonstration process was stopped by an halt command.
	 * 
	 * @return true if the demonstration was stopped
	 */
	public synchronized boolean isHalted() {
		return evalFlag == Prolog.HALT;
	}

    /**
     * Gets next solution
     *
     * @return the result of the demonstration
     * @throws NoMoreSolutionException if no more solutions are present
     * @see SolveInfo
     **/
    public synchronized SolveInfo solveNext() throws NoMoreSolutionException {
        if (evalFlag == TRUE_CP) {
            evalFlag = backTracking();
            SolveInfo sinfo;
            if (evalFlag == FALSE) {
                sinfo = new SolveInfo(startQuery);
            } else {
                evalFlag = eval();
                sinfo = new SolveInfo(startQuery,toVarArray(currentVars), currentGoal.copy());
            }
            if (evalFlag == FALSE || evalFlag == TRUE || evalFlag == HALT) {
                solveEnd();
            }
            QueryEvent ev = new QueryEvent(this,sinfo);
            notifyNewQueryResultAvailable(ev);
            return sinfo;
        } else
            throw new NoMoreSolutionException();
    }

    /**
     * Halts current solve computation
     */
    public void solveHalt() {
        mustStop = true;
    }

    /**
     * Accepts current solution
     */
    public synchronized void solveEnd() {
        theoryManager.transEnd(evalFlag == TRUE || evalFlag == TRUE_CP);
        theoryManager.optimize();
        Iterator it = currentLibraries.listIterator();
        while (it.hasNext()){
            ((Library) it.next()).onSolveEnd();
        }
    }

    /**
     * Unifies two terms using current demonstration context.
     *
     * @param t0 first term to be unified
     * @param t1 second term to be unified
     * @return true if the unification was successful
     */
    public synchronized boolean unify(Term t0, Term t1) {
        if (currentContext != null) {
            /*
             * ipothesis: if terms t0 and t1
             * are resolved, then they are
             * considered already contextualised in
             * current demonstration context.
             * So no renaming or resolving process for variable
             * is required.
             */
            currentContext.time = t0.resolveVariables(currentContext.time);
            currentContext.time = t1.resolveVariables(currentContext.time);
            return t0.unify(t1, currentContext.mark++);
        } else {
            return t0.unify(t1);
        }
    }

    /**
     * Gets a copy of a term with variables renamed, using current demostration
     * context.
     *
     * @param t0 the term to be copied and renamed
     * @return the copied and renamed term
     */
    public synchronized Term getRenamedCopy(Term t0) {
        if (currentContext != null) {
            /*
             * returns a copy contextualised in current
             * demonstration (so time value of the variables
             * inside the term are properly computed)
             */
            return currentContext.getRenamedTermCopy(t0);
        } else {
            return t0.copy();
        }
    }

    /**
     * Gets a term from a string, using the operators currently
     * defined by the engine
     *
     * @param st the string representing a term
     * @return the term parsed from the string
     * @throws InvalidTermException if the string does not represent a valid term
     */
    public synchronized Term toTerm(String st) throws InvalidTermException {
        return Term.parse(st, opManager);
    }

    /**
     * Gets the string representation of a term, using operators
     * currently defined by engine
     *
     * @param term      the term to be represented as a string
     * @return the string representing the term
     */
    public synchronized String toString(Term term) {
        return (term.toStringAsArgY(opManager, OperatorManager.OP_HIGH));
    }


    // inference core

    /*
     * demo process init.
     *
     * Evaluating goal g using the runtime context e
     * starting from timestamp t and marking m
     */
    private void init(RTContext e, Term g, int l, int t, int m) {
        spy(l, "Init: ", g);

        e.sDBTS = theoryManager.transStatus();
        e.startGoal = g;
        e.startLevel = l;
        e.startTime = t;
        e.startMark = m;
        e.evalState = false;
        e.compatibleGoals = new alice.util.LinkedList();
        e.goalsToEval = new alice.util.LinkedList();
        e.termsExecuted = new alice.util.LinkedList();
        e.nCompatibleGoals = 0;
        e.nGoalsToEval = 0;
        e.nTermsExecuted = 0;
        e.nOpenTermsExecuted = 0;
        e.time = t;
        e.mark = m;

        if (g.isStruct()) {
            // if it's not a built in -> finding in the dbase compatibles clauses
            Struct goal = (Struct) g;
            if (!goal.isBuiltIn()) {
                e.compatibleGoals = theoryManager.find(goal, t, m);
                e.nCompatibleGoals = e.compatibleGoals.length();
                spy(l, "Check type: " + e.nCompatibleGoals + " compatible rules found for ", goal);
                if (e.nCompatibleGoals != 0) {
                    e.evalState = true;
                    // get the first alternatives
                    e.loadChoice();
                }
                /*
                 * trho:
                 * warn if the goal if no clause with a compatible signature is defined and it is not declared dynamic
                 * 
                 * @author Tobias Rho 
                 */
                else {
                	try {
                		//System.out.println("check");
                		 if (warning){
                			 checkDynamic(t, m, goal);
                		 }
                	} catch(Exception ex) {
                		ex.printStackTrace();
                	}
                }

            } else {
                // it's a builtin, load as it is.
                e.evalState = true;
                e.currentGoal = new GoalInfo(goal);
                spy(l, "Check type: built-in ", goal);
                e.saveFGoal();
            }
        }
    }

    /**
     * checks for existance
     * 
     * @autor Tobias Rho
     * @param t
     * @param m
     * @param goal
     * @param functor
     */
	private void checkDynamic(int t, int m, Struct goal) {
		if(!goal.getName().equals("is_dynamic")) {
			LinkedList result = theoryManager.find(new Struct("is_dynamic", new Struct("/", 
					new Term[] {
						new Struct(goal.getName()), 
						new Int(goal.getArity())
			}
			))
			, t, m);
			if(result.length() == 0) {
				List list = new ArrayList();
				for(int i=0; i <goal.getArity();i++) {
					list.add(new Var());
				}
				Struct head = new Struct(goal.getName(), 
						(Term[])list.toArray(new Term[0]));
				result = theoryManager.find(
//						new Struct("clause",
//						new Term[] {
								head
//								new Var()
//						})
			 			, t, m);
			if(result.length() == 0) {
				
			      warn("could not find compatible goals: " + goal);
			}
			}
//			else
//			  System.out.println("Dynamic: "+ goal);
           	}
	}

    /**
     * Inference Algorithm: stepE (evaluation) stage
     *
     */
    private int stepE(RTContext e) {
        //e1')
        while (e.nGoalsToEval > 0) {
            if (mustStop) {
                return (FALSE);
            }
            e.loadFGoal();

            //------------------------------------
            // code inserted to allow evaluation of meta-clause as p(X):-X
            //
            //  e.currentGoal.goal=e.currentGoal.goal.getTerm();
            //
            //  When evaluating directly terms, they are
            //  dynamically identified in the case that
            //  there were variables.  This enables the
            //  dynamic linking of built-ins for terms
            //  coming from outside the demonstration context
            //
            Term goal_app = e.currentGoal.goal.getTerm();
            if (e.currentGoal.goal != goal_app) {
                identify(goal_app, false);
                e.currentGoal.goal = goal_app;
            }
            //------------------------------------

            spy(e.startLevel, "Eval: ", e.currentGoal.goal);

            if (!e.currentGoal.goal.isStruct()) {
                e.saveFGoal();
                return (FALSE);
            }

            Struct current_goal = (Struct) e.currentGoal.goal;

            int r;
            Term g0,g1,g2;

            if (!current_goal.isBuiltIn()) {
                /*
                 * if no open alternatives and no other term to execute -> current rtcontext
                 * no more needed -> reused to execute g subgoal => got TAIL RECURSION
                 * OPTIMIZATION!
                 */
                if (e.nGoalsToEval == 0 && !e.existChoice()) {
                    init(e, current_goal, e.startLevel, e.time, e.mark);
                    if (e.evalState) {
                        return (CONT);
                    } else {
                        return (FALSE);
                    }
                }
                // e3') point
                e.currentGoal.context = new RTContext();
                init(e.currentGoal.context, current_goal, e.startLevel + 1, e.time, e.mark);
                if (e.currentGoal.context.evalState) {
                    r = eval(e.currentGoal.context);
                    e.loadStatus();
                } else {
                    r = FALSE;
                }
            } else {

                int code = current_goal.getBuiltIn().getCode();
                // ',' (comma)  predicate
                if (code == 200) {
                    g1 = current_goal.getArg(1);
                    g0 = current_goal.getArg(0);
                    e.setGoal(g1);
                    e.saveFGoal();
                    e.setGoal(g0);
                    e.saveFGoal();
                    continue;
                } else {
                    // default case
                    // e2')
                    RTContext bak = currentContext;
                    currentContext = e;
                    r = builtin(e);
                    currentContext = bak;
                }
            }

            //e4')
            if (mustStop) {
                return (FALSE);
            }
            if (r == HALT) {
                return (HALT);
            }
            switch (r) {
                case FALSE:
                    e.currentGoal.goal.free(e.currentGoal.mark);
                    e.currentGoal.context = null;
                    e.saveFGoal();
                    return (FALSE);
                case TRUE:
                    e.currentGoal.context = null;
                case TRUE_CP:
                    e.saveTGoal();
                    break;
            }
        }
        //e5')
        return (e.existChoice() ? TRUE_CP : TRUE);
    }

    /**
     *  Backtracking stage
     *
     */
    private int stepB(RTContext e) {
        //b1')
        while (e.nTermsExecuted > 0) {
            if (mustStop) {
                return (FALSE);
            }
            e.loadTGoal();
            spy(e.startLevel, "Back: ", e.currentGoal.goal);
            //b2')
            int r;
            if (e.currentGoal.context != null) {
                r = backTracking(e.currentGoal.context);
                e.loadStatus();
            } else {
                r = FALSE;
            }
            //b3')
            if (mustStop) {
                return (FALSE);
            }
            switch (r) {
                case FALSE:
                    e.currentGoal.goal.free(e.currentGoal.mark);
                    e.currentGoal.context = null;
                    e.saveFGoal();
                    break;
                case TRUE:
                    e.currentGoal.context = null;
                case TRUE_CP:
                    e.saveTGoal();
                    return (CONT);
                case HALT:
                    return (HALT);
            }
        }
        //b4')
        e.startGoal.free(e.startMark);
        return (e.loadChoice() ? CONT : FALSE);
    }


    /**
     * Evaluation stage
     *
     */
    private int eval() {
        if (currentContext.evalState) {
            evalFlag = eval(currentContext);
        } else {
            evalFlag = FALSE;
        }
        return (evalFlag);
    }

    /**
     * Evaluation stage
     *
     * @param e the runtime context of the demonstration
     */
    private int eval(RTContext e) {
        int r;
        while (true) {
            while ((r = stepE(e)) == CONT) ;
            if (r != FALSE) {
                break;
            }
            if ((r = stepB(e)) != CONT) {
                break;
            }
        }
        return (r);
    }


    /**
     * backtracking stage
     *
     */
    private int backTracking() {
        if (currentContext.evalState) {
            evalFlag = backTracking(currentContext);
        } else {
            evalFlag = FALSE;
        }
        return (evalFlag);
    }

    /**
     * Inference algorithm: Backtracking stage
     *
     * @param e the runtime context of the demonstration
     */
    private int backTracking(RTContext e) {
        int r;
        while (true) {
            if ((r = stepB(e)) != CONT) {
                break;
            }
            while ((r = stepE(e)) == CONT) ;
            if (r != FALSE) {
                break;
            }
        }
        return (r);
    }

    /**
     * Evaluates builtin predicates
     *
     * @param e current runtime context of the demonstration
     * @return flag indicating the result of the evaluation
     */
    protected int builtin(RTContext e) {
        try {
            if (!e.currentGoal.goal.isStruct()) {
                return FALSE;
            }

            Struct goal = (Struct) e.currentGoal.goal;

            if (!goal.isBuiltIn()) {
                return FALSE;
            }

            BuiltIn g = goal.getBuiltIn();

            //  check if it's a builtin predicate from libraries
            if (g.isLibraryPredicate()) {
                boolean res = g.evalAsPredicate();
                return (res ? TRUE : FALSE);
            }

            //  check if it's a builtin functor from libraries
            if (g.isLibraryFunctor()) {
                Term term = g.evalAsFunctor();
                if (term != null) {
                    e.currentGoal.goal = term;
                    if (term.isStruct()) {
                        //-------------------------
                        identify(term, true);
                        //-------------------------
                        if (((Struct) term).isBuiltIn()) {
                            goal = (Struct) term;
                            g = goal.getBuiltIn();
                        } else {
                            return TRUE;
                        }
                    } else {
                        return TRUE;
                    }
                } else {
                    return FALSE;
                }
            }

            Term arg0,val0;
            Term arg1,val1;
            Term arg2;

            alice.util.LinkedList l;
            ClauseInfo c;
            int gcode = g.getCode();

            if (gcode < 100) {
                switch (gcode) {
                    // fail
                    case 0:
                        break;

                        // true
                    case 1:
                        return (TRUE);

                        // halt
                    case 2:
                        return (HALT);

                        // !, cut
                    case 3:
                        e.cut();
                        return (TRUE);

                        // $restore_db
                    case 4:
                        // restore theory to the state  before goal evaluation
                        theoryManager.transRestore(e.sDBTS);
                        return (TRUE);
                }
                return (FALSE);
            } else if (gcode < 200) {
                arg0 = goal.getArg(0).getTerm();
                switch (gcode) {
                    //$asserta
                    case 100:
                        if (!arg0.isStruct()) {
                            return FALSE;
                        }
                        theoryManager.assertA((Struct) arg0, true, null,false);
                        return (TRUE);

                        // $assertz
                    case 101:
                        if (!arg0.isStruct()) {
                            return FALSE;
                        }
                        theoryManager.assertZ((Struct) arg0, true, null,false);
                        return (TRUE);

                        // $retract
                    case 102:
                        if (!arg0.isStruct()) {
                            return FALSE;
                        }
                        Struct sarg0 = (Struct) arg0;

                        c = theoryManager.retract(sarg0, e.time, e.mark);
                        // if clause to retract found -> retract + true
                        if (c != null) {
                            Struct clause = null;
                            if (!sarg0.isClause()) {
                                clause = new Struct(":-", arg0, new Struct("true"));
                            } else {
                                clause = (sarg0);
                            }
                            clause.unify(c.clause, e.mark++);
                            e.time = c.timestamp;
                            return (TRUE);
                        }
                        return FALSE;
                        // halt/1
                    case 103:
                        if (!arg0.isNumber()) {
                            return FALSE;
                        }
                        haltCode = ((Number) arg0).intValue();
                        return (HALT);

                        /*
                        * loads a tuprolog library, given its java
                        * class name
                        */
                    case 104:
                        if (!arg0.isAtom()) {
                            return FALSE;
                        }
                        try {
                            loadLibrary(((Struct) arg0).getName());
                            return TRUE;
                        } catch (Exception ex) {
                            return FALSE;
                        }

                        /*
                        * unloads a tuprolog library, given its java
                        * class name
                        */
                    case 105:
                        if (!arg0.isAtom()) {
                            return FALSE;
                        }
                        try {
                            unloadLibrary(((Struct) arg0).getName());
                            return TRUE;
                        } catch (Exception ex) {
                            return FALSE;
                        }

                        /*
                         * get flag list:  flag_list(-List)
                         */
                    case 106:
                        Struct flist = new Struct();
                        java.util.Iterator it = flags.iterator();
                        while (it.hasNext()) {
                            Flag fl = (Flag) it.next();
                            flist = new Struct(new Struct("flag", new Struct(fl.getName()), fl.getValue()), flist);
                        }
                        boolean res = unify(arg0, flist);
                        return res ? TRUE : FALSE;
                }
                return (FALSE);
            } else if (gcode < 300) {
                arg0 = goal.getArg(0).getTerm();
                arg1 = goal.getArg(1).getTerm();
                switch (gcode) {

                    // is
                    case 201:

                        if (arg1.isStruct()) {
                            Struct t = (Struct) arg1;
                            //----------------------------------
                            // identify the second argument in the case
                            // that it is a variable
                            //
                            if (arg1 != goal.getArg(1)) {
                                identify(t, true);
                            }
                            //-----------------------------------
                            if (t.isBuiltIn()) {
                                BuiltIn bt = t.getBuiltIn();
                                // check for library functors
                                if (bt.isLibraryFunctor()) {
                                    val1 = bt.evalAsFunctor();
                                } else {
                                    val1 = arg1;
                                }
                            } else {
                                val1 = arg1;
                            }
                        } else {
                            val1 = arg1;
                        }

                        if (/*val1.isNumber() &&*/ arg0.unify(val1, e.mark++)) {
                            return (TRUE);
                        }
                        break;

                        // =
                    case 202:
                        if (arg0.unify(arg1, e.mark++)) {
                            return (TRUE);
                        }
                        break;

                        // \\=
                    case 203:
                        if (!arg0.unify(arg1, e.mark++)) {
                            return (TRUE);
                        }
                        break;

                        // $tolist
                    case 204:
                        // transform arg0 to a list, unify it with arg1
                        if (!arg0.isStruct()) {
                            break;
                        }
                        val0 = ((Struct) arg0).toList();
                        if (val0 == null || !arg1.unify(val0, e.mark++)) {
                            break;
                        }
                        return (TRUE);

                        // $fromlist
                    case 205:
                        // get the compound representation of the list
                        // provided as arg1, and unify it with arg0
                        if (!arg1.isList()) {
                            break;
                        }
                        val1 = ((Struct) arg1).fromList();
                        if (val1 == null || !arg0.unify(val1, e.mark++)) {
                            break;
                        }
                        return (TRUE);

                        // $copy
                    case 206:
                        // unify arg1 with a renamed copy of arg0
                        if (arg1.unify(e.getRenamedTermCopy(arg0), e.mark++)) {
                            return (TRUE);
                        }
                        break;

                        // $append
                    case 207:
                        // append arg0 to arg1
                        if (!arg1.isList()) {
                            break;
                        }
                        ((Struct) arg1).append(arg0);
                        return (TRUE);

                        // $find
                    case 208:
                        // look for clauses whose head unifies whith arg0 and enqueue them to list arg1
                        if (/*!arg0.isStruct() ||*/ !arg1.isList()) {
                            break;
                        }
                        l = theoryManager.find(arg0, e.time, e.mark);
                        while (!l.isEmptyList()) {
                            ClauseInfo b = (ClauseInfo) l.head;
                            l = l.tail;
                            ((Struct) arg1).append(b.clause);
                            if (b.timestamp > e.time) {
                                e.time = b.timestamp;
                            }
                        }
                        return (TRUE);


                        // flag(@Name,+Value)
                    case 209:
                        if (!arg0.isStruct() || !arg1.isGround()) {
                            break;
                        }
                        String name = arg0.toString();
                        Iterator it = flags.iterator();
                        while (it.hasNext()) {
                            Flag flag = (Flag) it.next();
                            if (flag.getName().equals(name)) {
                                if (flag.isModifiable() && flag.isValidValue(arg1)) {
                                    flag.setValue(arg1);
                                    return (TRUE);
                                } else {
                                    return FALSE;
                                }
                            }
                        }
                        break;
                }
                return (FALSE);
            } else if (gcode < 400) {
                arg0 = goal.getArg(0).getTerm();
                arg1 = goal.getArg(1).getTerm();
                arg2 = goal.getArg(2).getTerm();
                switch (gcode) {
                    // op
                    case 300:
                        /*
                        op(+Precedence, +Type, +Name) defines a new operator
                        if precedence not in 0..1200 -> delete currently present op
                         */
                        if (!arg0.isNumber() || !arg1.isStruct() || !arg2.isStruct()) {
                            break;
                        }
                        opManager.opNew(((Struct) arg2).getName(), ((Struct) arg1).getName(), ((Number) arg0).intValue());
                        return (TRUE);
                }
                return (FALSE);
            }
            return (FALSE);
        } catch (NullPointerException ex) {
            return FALSE;
        } catch (Exception ex) {
            ex.printStackTrace();
            return (FALSE);
        }
    }

    /**
     * Identifies the term passed as argument.
     *
     * This involves identifying structs representing builtin
     * predicates and functors, and setting up related structures and links
     *
     * @parm term the term to be identified
     */
    protected void identify(Term term, boolean forceArgumentIdentification) {
        //System.out.println("IDENTIFYING "+term);
        if (term == null) {
            return;
        }
        term = term.getTerm();
        if (!term.isStruct()) {
            return;
        }
        Struct t = (Struct) term;

        int arity = t.getArity();
        //------------------------------------------
        for(int c = 0;c < arity;c++){
            identify(t.getArg(c),true);
        }
        //------------------------------------------
        //
        //  The identification is forced (recursively)
        //  for the second argument of the 'is' built-in.
        //
        //  In the other cases, the arguments are identified
        //  only if the structure is a Clause or ',' structure.
        //
        /*
        if (forceArgumentIdentification) {
            for (int c = 0; c < arity; c++) {
                identify(t.getArg(c), true);
            }
        } else {
            String tname = t.getName();
            if (t.isClause() || tname.equals(",")) {
                for (int c = 0; c < arity; c++) {
                    identify(t.getArg(c), false);
                }
            } else if (tname.equals("is")) {
                identify(t.getTerm(1), true);
            }
        }
        */
        //------------------------------------------

        try {
            Object lib = extPredicates.get(t.getName() + "_" + arity);
            if (lib != null) {
                t.setBuiltIn(new BuiltIn(BuiltIn.LIBRARY_PREDICATE, t, (Library) lib));
                return;
            } else {
                lib = extFunctors.get(t.getName() + "_" + arity);
                if (lib != null) {
                    t.setBuiltIn(new BuiltIn(BuiltIn.LIBRARY_FUNCTOR, t, (Library) lib));
                    return;
                }
            }
        } catch (Exception ex) {
        }

        for (int c = 0; c < BUILTIN.length; c++) {
            for (int d = 0; d < BUILTIN[c].length; d++) {
                String st = BUILTIN[c][d];
                if (arity == c && t.getName().equals(st)) {
                    t.setBuiltIn(new BuiltIn(c * 100 + d));
                    return;
                }
            }
        }
    }

    /**
     * Defines a new flag
     */
    boolean defineFlag(String name, Struct valueList, Term defValue, boolean modifiable, String libName) {
        flags.add(new Flag(name, valueList, defValue, modifiable, libName));
        return true;
    }


    /**
     * Creates an array of variables from a linked list of variables
     */
    private Var[] toVarArray(alice.util.LinkedList vars) {
        if (vars != null) {
            int size = vars.length();
            Var[] bindings = new Var[size];
            alice.util.LinkedList l = vars;
            for (int i = 0; i < size; i++) {
                bindings[i] = (Var) l.head;
                l = l.tail;
            }
            return bindings;
        } else {
            return new Var[0];
        }
    }

    // spy interface ----------------------------------------------------------

    /**
     * Switches on/off the notification of spy information events
     *
     * @param state - true for enabling the notification of spy event
     */
    public synchronized void setSpy(boolean state) {
        spy = state;
    }

    /**
     * Checks the spy state of the engine
     *
     * @return true if the engine emits spy information
     */
    public synchronized boolean isSpy() {
        return spy;
    }


    /**
     * Notifies a spy information event
     */
    protected void spy(String s) {
        if (spy) {
            notifySpy(new SpyEvent(this, s));
        }
    }

    /**
     * Notifies a spy information event
     */
    protected void spy(int i, String s, Term g) {
        //System.out.println("spy: "+i+"  "+s+"  "+g);
        if (spy) {
            notifySpy(new SpyEvent(this, "spy: " + i + "  " + s + "  " + g));
        }
    }


    /**
     * Switches on/off the notification of warning information events
     *
     * @param state - true for enabling warning information notification
     */
    public synchronized void setWarning(boolean state) {
        warning = state;
    }

    /**
     * Checks if warning information are notified
     *
     * @return true if the engine emits warning information
     */
    public synchronized boolean isWarning() {
        return warning;
    }

    /**
     * Notifies a warn information event
     *
     *
     * @param m the warning message
     */
    public void warn(String m) {
        if (warning){
            notifyWarning(new WarningEvent(this, m));
        }
    }


    /**
     * Produces an output information event
     *
     * @param m the output string
     */
    public synchronized void stdOutput(String m) {
        notifyOutput(new OutputEvent(this, m));
    }

    // event listeners management

    /**
     * Adds a listener to ouput events
     *
     * @param l the listener
     */
    public synchronized void addOutputListener(OutputListener l) {
        outputListeners.add(l);
    }


    /**
     * Adds a listener to theory events
     *
     * @param l the listener
     */
    public synchronized void addTheoryListener(TheoryListener l) {
        theoryListeners.add(l);
    }
    
    /**
     * Adds a listener to library events
     *
     * @param l the listener
     */
    public synchronized void addLibraryListener(LibraryListener l) {
        libraryListeners.add(l);
    }
    
    /**
     * Adds a listener to theory events
     *
     * @param l the listener
     */
    public synchronized void addQueryListener(QueryListener l) {
        queryListeners.add(l);
    }
    
    /**
     * Adds a listener to spy events
     *
     * @param l the listener
     */
    public synchronized void addSpyListener(SpyListener l) {
        spyListeners.add(l);
    }

    /**
     * Adds a listener to warning events
     *
     * @param l the listener
     */
    public synchronized void addWarningListener(WarningListener l) {
        warningListeners.add(l);
    }

    /**
     * Removes a listener to ouput events
     *
     * @param l the listener
     */
    public synchronized void removeOutputListener(OutputListener l) {
        outputListeners.remove(l);
    }

    /**
     * Removes all output event listeners
     */
    public synchronized void removeAllOutputListeners() {
        outputListeners.clear();
    }

    /**
     * Removes a listener to theory events
     *
     * @param l the listener
     */
    public synchronized void removeTheoryListener(TheoryListener l) {
        theoryListeners.remove(l);
    }

    /**
     * Removes a listener to library events
     *
     * @param l the listener
     */
    public synchronized void removeLibraryListener(LibraryListener l) {
        libraryListeners.remove(l);
    }
    
    /**
     * Removes a listener to query events
     *
     * @param l the listener
     */
    public synchronized void removeQueryListener(QueryListener l) {
        queryListeners.remove(l);
    }

    
    /**
     * Removes a listener to spy events
     *
     * @param l the listener
     */
    public synchronized void removeSpyListener(SpyListener l) {
        spyListeners.remove(l);
    }

    /**
     * Removes all spy event listeners
     */
    public synchronized void removeAllSpyListeners() {
        spyListeners.clear();
    }

    /**
     * Removes a listener to warning events
     *
     * @param l the listener
     */
    public synchronized void removeWarningListener(WarningListener l) {
        warningListeners.remove(l);
    }

    /**
     * Removes all warning event listeners
     */
    public synchronized void removeAllWarningListeners() {
        warningListeners.clear();
    }

    /**
     * Gets a copy of current listener list to output events
     */
    public synchronized List getOutputListenerList() {
        return (List) outputListeners.clone();
    }

    /**
     * Gets a copy of current listener list to warning events
     *
     */
    public synchronized List getWarningListenerList() {
        return (List) warningListeners.clone();
    }

    /**
     * Gets a copy of current listener list to spy events
     *
     */
    public synchronized List getSpyListenerList() {
        return (List) outputListeners.clone();
    }

    /**
     * Gets a copy of current listener list to theory events
     * 
     */
    public synchronized List getTheoryListenerList() {
        return (List) theoryListeners.clone();
    }

    /**
     * Gets a copy of current listener list to library events
     *
     */
    public synchronized List getLibraryListenerList() {
        return (List) libraryListeners.clone();
    }

    /**
     * Gets a copy of current listener list to query events
     *
     */
    public synchronized List getQueryListenerList() {
        return (List) queryListeners.clone();
    }

    // notification

    /**
     * Notifies an ouput information event
     *
     * @param e the event
     */
    protected void notifyOutput(OutputEvent e) {
        Iterator it = outputListeners.listIterator();
        while (it.hasNext()){
            ((OutputListener) it.next()).onOutput(e);
        }
    }

    /**
     * Notifies a spy information event
     *
     * @param e the event
     */
    protected void notifySpy(SpyEvent e) {
        Iterator it = spyListeners.listIterator();
        while (it.hasNext()){
            ((SpyListener) it.next()).onSpy(e);
        }
    }

    /**
     * Notifies a warning information event
     *
     * @param e the event
     */
    protected void notifyWarning(WarningEvent e) {
        Iterator it = warningListeners.listIterator();
        while (it.hasNext()){
            ((WarningListener) it.next()).onWarning(e);
        }
    }


    //
    
    /**
     * Notifies a new theory set or updated event
     * 
     * @param e the event
     */
    protected void notifyChangedTheory(TheoryEvent e) {
        Iterator it = theoryListeners.listIterator();
        while (it.hasNext()){
            ((TheoryListener) it.next()).theoryChanged(e);
        }
    }

    /**
     * Notifies a library loaded event
     * 
     * @param e the event
     */
    protected void notifyLoadedLibrary(LibraryEvent e) {
        Iterator it = libraryListeners.listIterator();
        while (it.hasNext()){
            ((LibraryListener) it.next()).libraryLoaded(e);
        }
    }

    /**
     * Notifies a library unloaded event
     * 
     * @param e the event
     */
    protected void notifyUnloadedLibrary(LibraryEvent e) {
        Iterator it = libraryListeners.listIterator();
        while (it.hasNext()){
            ((LibraryListener) it.next()).libraryUnloaded(e);
        }
    }

    /**
     * Notifies a library loaded event
     * 
     * @param e the event
     */
    protected void notifyNewQueryResultAvailable(QueryEvent e) {
        Iterator it = queryListeners.listIterator();
        while (it.hasNext()){
            ((QueryListener) it.next()).newQueryResultAvailable(e);
        }
    }
    
}