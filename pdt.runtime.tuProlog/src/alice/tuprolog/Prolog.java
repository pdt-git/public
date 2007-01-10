/*
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
import java.util.Iterator;
import java.util.List;

import alice.tuprolog.event.LibraryEvent;
import alice.tuprolog.event.LibraryListener;
import alice.tuprolog.event.OutputEvent;
import alice.tuprolog.event.OutputListener;
import alice.tuprolog.event.QueryEvent;
import alice.tuprolog.event.QueryListener;
import alice.tuprolog.event.SpyEvent;
import alice.tuprolog.event.SpyListener;
import alice.tuprolog.event.TheoryEvent;
import alice.tuprolog.event.TheoryListener;
import alice.tuprolog.event.WarningEvent;
import alice.tuprolog.event.WarningListener;



/**
 *
 * The Prolog class represents a tuProlog engine.
 *
 */
public class Prolog implements Serializable {
	
	// 2P version
	private static final String VERSION = "2.0";    
	
	/*  manager of current theory */
	private TheoryManager theoryManager;
	/*  component managing primitive  */
	private PrimitiveManager primitiveManager;    
	/* component managing operators */
	private OperatorManager opManager;    
	/* component managing flags */
	private FlagManager flagManager;
	/* component managing libraries */
	private LibraryManager libraryManager;
	/* component managing engine */
	private EngineManager engineManager;
	
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
	private Prolog(boolean spy, boolean warning) {
		outputListeners = new ArrayList();
		spyListeners = new ArrayList();
		warningListeners = new ArrayList();
		this.spy = spy;
		this.warning = warning;
		theoryListeners = new ArrayList();
		queryListeners = new ArrayList();
		libraryListeners = new ArrayList();
		initializeManagers();
	}
	
	
	private void initializeManagers() {
		flagManager      = new FlagManager();
		libraryManager   = new LibraryManager();		
		opManager        = new OperatorManager();
		theoryManager    = new TheoryManager();
		primitiveManager = new PrimitiveManager();
		engineManager    = new EngineManager();
		//config managers
		theoryManager.initialize(this);
		libraryManager.initialize(this);
		flagManager.initialize(this);
		primitiveManager.initialize(this);
		engineManager.initialize(this);
	}
	
	
	/** Gets the component managing flags */
	FlagManager getFlagManager() {
		return flagManager;
	}
	
	/** Gets the component managing theory */
	TheoryManager getTheoryManager() {
		return theoryManager;
	}
	
	/** Gets the component managing primitives */
	PrimitiveManager getPrimitiveManager() {
		return primitiveManager;
	}
	
	/** Gets the component managing libraries */
	LibraryManager getLibraryManager() {
		return libraryManager;
	}
	
	/** Gets the component managing operators */
	OperatorManager getOperatorManager() {
		return opManager; 
	}
	
	/** Gets the component managing engine */
	EngineManager getEngineManager() {
		return engineManager; 
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
		theoryManager.solveTheoryGoal();
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
		theoryManager.solveTheoryGoal();
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
		return libraryManager.loadLibrary(className);
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
		libraryManager.loadLibrary(lib);
	}
	
	
	/**
	 * Gets the list of current libraries loaded
	 *
	 * @return the list of the library names
	 */
	public synchronized String[] getCurrentLibraries() {
		return libraryManager.getCurrentLibraries();
	}
	
	
	/**
	 * Unloads a previously loaded library
	 *
	 * @param name of the library to be unloaded
	 * @throws InvalidLibraryException if name is not a valid loaded library
	 */
	public synchronized void unloadLibrary(String name) throws InvalidLibraryException {
		libraryManager.unloadLibrary(name);
	}
	
	
	/**
	 * Gets the reference to a loaded library
	 *
	 * @param name the name of the library already loaded
	 * @return the reference to the library loaded, null if the library is
	 *         not found
	 */
	public synchronized Library getLibrary(String name) {
		return libraryManager.getLibrary(name);
	}
	
	
	protected Library getLibraryPredicate(String name, int nArgs) {
		return primitiveManager.getLibraryPredicate(name,nArgs);
	}
	
	
	protected Library getLibraryFunctor(String name, int nArgs) {
		return primitiveManager.getLibraryFunctor(name,nArgs);
	}
	
	
	
	// operators management
	
	/**
	 *  Gets the list of the operators currently defined
	 *
	 *  @return the list of the operators
	 */
	public synchronized java.util.List getCurrentOperatorList() {
		return opManager.getOperators();
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
		if (g == null) return null;
		
		SolveInfo sinfo = engineManager.solve(g);
		
		QueryEvent ev = new QueryEvent(this,sinfo);
		notifyNewQueryResultAvailable(ev);
		
		return sinfo;
		
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
			Term t = Term.parseSentence(st,opManager);
			return solve(t);
		} catch (InvalidTermException ex) {
			throw new MalformedGoalException();
		}
	}
	
	/**
	 * Gets next solution
	 *
	 * @return the result of the demonstration
	 * @throws NoMoreSolutionException if no more solutions are present
	 * @see SolveInfo
	 **/
	public synchronized SolveInfo solveNext() throws NoMoreSolutionException {
		if (hasOpenAlternatives()) {
			SolveInfo sinfo = engineManager.solveNext();
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
		engineManager.solveHalt();
	}
	
	/**
	 * Accepts current solution
	 */
	public synchronized void solveEnd() {
		engineManager.solveEnd();
	}
	
	
	/**
	 * Asks for the presence of open alternatives to be explored
	 * in current demostration process.
	 *
	 * @return true if open alternatives are present
	 */
	public synchronized boolean hasOpenAlternatives() {
		return engineManager.hasOpenAlternatives();
	}
	
	/**
	 * Checks if the demonstration process was stopped by an halt command.
	 * 
	 * @return true if the demonstration was stopped
	 */
	public synchronized boolean isHalted() {
		return engineManager.isHalted();
	}
	
	/**
	 * Unifies two terms using current demonstration context.
	 *
	 * @param t0 first term to be unified
	 * @param t1 second term to be unified
	 * @return true if the unification was successful
	 */
	public synchronized boolean match(Term t0, Term t1) {
		return t0.match(t1);
	}
	
	/**
	 * Unifies two terms using current demonstration context.
	 *
	 * @param t0 first term to be unified
	 * @param t1 second term to be unified
	 * @return true if the unification was successful
	 */
	public synchronized boolean unify(Term t0, Term t1) {
		return t0.unify(this,t1);
	}
	
	/**
	 * Identify functors
	 * 
	 * @param term term to identify
	 */
	public synchronized void identifyFunctor(Term term) {
		primitiveManager.identifyFunctor(term);
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
	
	
	/**
	 * Defines a new flag
	 */
	boolean defineFlag(String name, Struct valueList, Term defValue, boolean modifiable, String libName) {
		return flagManager.defineFlag(name,valueList,defValue,modifiable,libName);
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
	 * @param s TODO
	 */
	protected void spy(String s, Engine e) {
		//System.out.println("spy: "+i+"  "+s+"  "+g);
		if (spy) {
			ExecutionContext ctx = e.currentContext;
			int i=0;
			String g = "-";
			if (ctx.fatherCtx != null){
				i = ctx.depth-1;
				g = ctx.fatherCtx.currentGoal.toString();
			}
			notifySpy(new SpyEvent(this, e, "spy: " + i + "  " + s + "  " + g));
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
			//log.warn(m);
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
		return (List) spyListeners.clone();
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
		while (it.hasNext()) {
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
		while (it.hasNext()) {
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
		while (it.hasNext()) {
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
		while (it.hasNext()) {
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
		while (it.hasNext()) {
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
		while (it.hasNext()) {
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
		while (it.hasNext()) {
			((QueryListener) it.next()).newQueryResultAvailable(e);
		}
	}
	
	
}