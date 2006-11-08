/*
 *
 *
 */
package alice.tuprolog;

import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;



/**
 * @author Alex Benini
 *
 * Core engine
 */
public class EngineManager implements java.io.Serializable {
	
	Prolog			mediator;
	private TheoryManager    theoryManager;
	private PrimitiveManager primitiveManager;
	private LibraryManager   libraryManager;
	
	
	/* Current environment */
	Engine env;
	/* Last environment used */
	private Engine last_env;
	/* Stack environments of nidicate solving */
	private LinkedList stackEnv = new LinkedList();
	
	private SolveInfo sinfo;
	
	
	/** States */
	final State INIT;
	final State GOAL_EVALUATION;
	final State RULE_SELECTION;
	final State GOAL_SELECTION;
	final State BACKTRACK;
	final State END_FALSE;
	final State END_TRUE;
	final State END_TRUE_CP;
	final State END_HALT;
	
	public static final int HALT    = -1;
	public static final int FALSE   =  0;
	public static final int TRUE    =  1;
	public static final int TRUE_CP =  2;
	
	
	public EngineManager() {
		/* Istanzio gli stati */
		INIT            = new StateInit(this);
		GOAL_EVALUATION = new StateGoalEvaluation(this);
		RULE_SELECTION  = new StateRuleSelection(this);
		GOAL_SELECTION  = new StateGoalSelection(this);
		BACKTRACK       = new StateBacktrack(this);
		END_FALSE       = new StateEnd(this,FALSE);
		END_TRUE        = new StateEnd(this,TRUE);
		END_TRUE_CP     = new StateEnd(this,TRUE_CP);
		END_HALT        = new StateEnd(this,HALT);
	}
	
	
	/**
	 * Config this Manager
	 */
	void initialize(Prolog vm) {
		mediator			= vm;
		theoryManager    = vm.getTheoryManager();
		primitiveManager = vm.getPrimitiveManager();
		libraryManager   = vm.getLibraryManager();
	}
	
	void spy(String action, Engine env) {
		mediator.spy(action,env);
	}
	
	/**
	 *  Solves a query
	 *
	 * @param g the term representing the goal to be demonstrated
	 * @return the result of the demonstration
	 * @see SolveInfo
	 **/
	public SolveInfo solve(Term query) {
		try {
			query.resolveTerm();
			
			libraryManager.onSolveBegin(query);
			primitiveManager.identifyPredicate(query);
			theoryManager.transBegin();
			
			freeze();
			env = new Engine(this, query);
			StateEnd result = env.run();
			defreeze();
			
			sinfo = new SolveInfo(
					env.query,
					result.getResultGoal(),
					result.getResultDemo(),
					result.getResultVars()
			);
			if (!sinfo.hasOpenAlternatives()) solveEnd();
			return sinfo;
		} catch (Exception ex) {
			ex.printStackTrace();
			return new SolveInfo(query);
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
			refreeze();
			env.nextState = BACKTRACK;
			StateEnd result = env.run();
			defreeze();
			sinfo = new SolveInfo(
					env.query,
					result.getResultGoal(),
					result.getResultDemo(),
					result.getResultVars()
			);
			if (!sinfo.hasOpenAlternatives()) solveEnd();
			return sinfo;
		} else
			throw new NoMoreSolutionException();
	}
	
	/**
	 * Halts current solve computation
	 */
	public void solveHalt() {
		env.mustStop();
	}
	
	/**
	 * Accepts current solution
	 */
	public void solveEnd() {
		theoryManager.transEnd(sinfo.isSuccess());
		theoryManager.optimize();
		libraryManager.onSolveEnd();
	}
	
	
	private void freeze() {
		if(env==null) return;
		try {
			if (stackEnv.getLast()==env) return;
		} catch(NoSuchElementException e) {}
		stackEnv.addLast(env);
	}
	
	private void refreeze() {
		freeze();
		env = last_env;    		
	}
	
	private void defreeze() {
		last_env = env;
		if (stackEnv.isEmpty()) return;
		env = (Engine)(stackEnv.removeLast());
	}
	
	
	/*
	 * Utility functions for Finite State Machine
	 */
	
	LinkedList find(Term t) {
		return theoryManager.find(t);
	}
	
	void identify(Term t) {
		primitiveManager.identifyPredicate(t);
	}
	
	List saveLastTheoryStatus() {
		return theoryManager.transStatus();
	}
	
	List restoreLastTheoryStatus() {
		return ((ChoicePointContext)env.choicePointSelector.fetch()).theoryTransientClauses;
	}
	
	void pushSubGoal(SubGoalTree goals) {
		env.currentContext.goalsToEval.pushSubGoal(goals);
	}
	
	
	void cut() {
		env.choicePointSelector.cut(env.currentContext.choicePointAfterCut);
	}
	
	
	ExecutionContext getCurrentContext() {
		return (env==null)? null : env.currentContext;
	}
	
	
	/**
	 * Asks for the presence of open alternatives to be explored
	 * in current demostration process.
	 *
	 * @return true if open alternatives are present
	 */
	boolean hasOpenAlternatives() {
		if (sinfo==null) return false;
		return sinfo.hasOpenAlternatives();
	}
	
	
	/**
	 * Checks if the demonstration process was stopped by an halt command.
	 * 
	 * @return true if the demonstration was stopped
	 */
	boolean isHalted() {
		if (sinfo==null) return false;
		return sinfo.isHalted();
	}
	
	
}