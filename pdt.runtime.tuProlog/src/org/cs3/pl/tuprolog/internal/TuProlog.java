package org.cs3.pl.tuprolog.internal;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;

import alice.tuprolog.InvalidLibraryException;
import alice.tuprolog.InvalidTheoryException;
import alice.tuprolog.Library;
import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.NoMoreSolutionException;
import alice.tuprolog.Prolog;
import alice.tuprolog.event.QueryListener;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.event.SpyListener;
import alice.tuprolog.Theory;
import alice.tuprolog.event.WarningEvent;
import alice.tuprolog.event.WarningListener;

/**
 * Global PrologInterface TuProlog properties. 
 * 
 * @author Tobias rho
 *
 */
public class TuProlog {
    public final static String FQN = Factory.class.getName();
	private Prolog engine;

    /**
     * Inits the Engine
     * 
     * @throws IOException 
     * @throws InvalidTheoryException 
     * @throws InvalidTheoryException
     * @throws IOException
     */
    
    public TuProlog() throws InvalidTheoryException, IOException {
    	engine = new Prolog();
    	initEngine();
    }
    
    /**
     * Make dynamic/1 and multifile/1 compatible
     * and load swi and tu compatibility libraries.
     * 
     * @throws InvalidTheoryException
     * @throws IOException
     */
	public void initEngine() throws InvalidTheoryException, IOException {
		Theory theory = new Theory(
				":- op(1150, fx, dynamic). \n" +
				":- op(1150, fx, multifile).\n"+
				
				"dynamic(A) :- is_dynamic(A),!.\n" +
				"dynamic(A) :- assert(is_dynamic(A)).\n" + 
				"multifile(A) :- is_multifile(A),!.\n" +
				"multifile(A) :- assert(is_multifile(A)).\n"
				);
		engine.addTheory(theory);
		
		theory = new Theory(
				":- dynamic is_dynamic/1.\n"+
				":- dynamic is_multifile/1.\n"+
				":- dynamic member/2.\n"+
				":- dynamic apppend/3.\n"
			);
		engine.addTheory(theory);
		loadLibrary("compatiblitySWI.pl");
		loadLibrary("compatiblityTU.pl");
		loadLibrary("javaFactbase.pl");
//		
		engine.setWarning(true);
		engine.addWarningListener(new WarningListener() {
			public void onWarning(WarningEvent e) {
				System.out.println(e.getMsg());
			}
		});
	}

	
	/**
	 * Load self-contained prolog files relative to the
	 * resources directory. 
	 * 
	 * @author Tobias Rho
	 * @param library
	 * @throws InvalidTheoryException
	 * @throws IOException
	 */
	public void loadLibrary(String library) throws InvalidTheoryException, IOException {
		engine.addTheory(new Theory(
    			new BufferedInputStream(
    					engine.getClass().getClassLoader().getResourceAsStream("./resources/"+ library)
    			)));
	}
	
	/**
	 * Load Java-based prolog library to the current engine.
	 * 
	 * @author Hasan Abdel Halim
	 * @param className name of the Java class containing the library to be loaded.
	 * @throws InvalidLibraryException
	 */
	public Library loadLibraryClass(String className) throws InvalidLibraryException{
		return engine.loadLibrary(className);
	}

	/**
	 * 
	 * TODO add predicate to ignore module:-
	 * TODO add predicate to ignore model:functor and execute functor
	 * TODO create ObserverLibrary in the same way as PrologEventDispatcher 
	 */
	
	/**
	 * Answers a stream on a resource found by looking up resName
	 * in the resource folder.
	 * resName may also be a subdirectory and a file e.g. "image/map.gif".
	 *  
	 * @param resName
	 * @return
	 */
    public InputStream getFileContentFromResourceFolder(String resName) {
    	return  new BufferedInputStream(
    	engine.getClass().getClassLoader().
        getResourceAsStream("./resources/"+ resName));
    }
    
	public boolean isHalted() {
		return engine.isHalted();
	}


	/**
	 * @see Prolog#clearTheory()
	 * @param listener
	 */
	public void clearTheory() {
		engine.clearTheory();
	}

	/**
	 * @see Prolog#addSpyListener(SpyListener)
	 * @param listener
	 */
	public void addSpyListener(SpyListener listener) {
		engine.addSpyListener(listener);
	}
	
	/**
	 * @see Prolog#addWarningListener(WarningListener)
	 * @param listener
	 */
	public void addWarningListener(WarningListener listener) {
		engine.addWarningListener(listener);	
	}

	/**
	 * @see Prolog#addQueryListener(QueryListener)
	 * @param listener
	 */
	public void addQueryListener(QueryListener listener) {
		engine.addQueryListener(listener);	
	}
	
	/**
	 * @see Prolog#solve(String)
	 * @param listener
	 */	
	public SolveInfo solve(String query) throws MalformedGoalException {
		return engine.solve(query);
	}


	/**
	 * @see Prolog#addTheory(Theory)
	 * @param listener
	 */
	public void addTheory(Theory theory) throws InvalidTheoryException {
		engine.addTheory(theory);
		
	}


	/**
	 * @see Prolog#solveNext()
	 * @param listener
	 */
	public SolveInfo solveNext() throws NoMoreSolutionException {
		return engine.solveNext();
	}

	/**
	 * @see Prolog#setWarning(boolean)
	 * @param listener
	 */
	public void setWarning(boolean b) {
		engine.setWarning(b);
	}
	
	/**
	 * @see Prolog#setSpy(boolean)
	 * @param listener
	 */
	public void setSpy(boolean b) {
		engine.setSpy(b);
	}

	/**
	 * @see Prolog#setTheory(Theory)
	 * @param listener
	 */
	public void setTheory(Theory th) throws InvalidTheoryException {
		engine.setTheory(th);
	}


	/**
	 * @see Prolog#solveEnd()
	 * @param listener
	 */
	public void solveEnd() {
		engine.solveEnd();
		
	}

    /**
	 * @see Prolog#isWarning()
     */
    public synchronized boolean isWarning() {
        return engine.isWarning();
    }
}
