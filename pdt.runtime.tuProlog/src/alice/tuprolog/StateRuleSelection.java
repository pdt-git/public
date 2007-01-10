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

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import alice.tuprolog.ClauseInfo;
import alice.tuprolog.Int;
import alice.tuprolog.Struct;
import alice.tuprolog.event.WarningEvent;
import alice.util.OneWayList;

/**
 * @author Alex Benini
 *
 */
public class StateRuleSelection extends State {
	
	
	/**
     * checks for existance
     * 
     * @autor Tobias Rho
     * @param t
     * @param m
     * @param goal
     * @param functor
     */
	private void checkDynamic(Struct goal) {
		
		if(!goal.getName().equals("is_dynamic")) {
			
			SolveInfo result = c.solve(
				new Struct("is_dynamic", 
					new Struct("/",  new Term[] {
						new Struct(goal.getName()), 
						new Int(goal.getArity())
			}
			)));
			if(!result.isSuccess()) {
				List list = new ArrayList();
				for(int i=0; i <goal.getArity();i++) {
					list.add(new Var());
				}
				Struct head = new Struct(goal.getName(), 
						(Term[])list.toArray(new Term[0]));
				LinkedList clauses = c.find(head);
				
				if(clauses.size() == 0) {
					
					c.mediator.notifyWarning(
				        new WarningEvent(goal,
				        		         "could not find compatible goals: " + goal));
				}
			}
         }
	}
	
	public StateRuleSelection(EngineManager c) {
		this.c = c;
		stateName = "Init";
	}
	
	/* (non-Javadoc)
	 * @see alice.tuprolog.AbstractRunState#doJob()
	 */
	void doJob(Engine e) {
		/*----------------------------------------------------
		 * Individuo compatibleGoals e
		 * stabilisco se derivo da Backtracking.
		 */
		Struct goal = e.currentContext.currentGoal;
		boolean fromBacktracking = true;
		ClauseStore clauseStore = e.clauseSelector;
		e.clauseSelector = null;
		if (clauseStore == null) {
			/* from normal evaluation */
			fromBacktracking = false;
			List varsList = new ArrayList();
			e.currentContext.trailingVars = new OneWayList(varsList,e.currentContext.trailingVars);
			clauseStore = ClauseStore.build(goal, varsList, c.find(goal));
			if (clauseStore == null){
				try {
					/*
	                 * trho:
	                 * warn if the goal if no clause with a compatible 
	                 * signature is defined and it is not declared dynamic
	                 * 
	                 * @author Tobias Rho 
	                 */
            		 if (true){
            			 checkDynamic(goal);
            		 }
            	} catch(Exception ex) {
            		ex.printStackTrace();
            	}
				e.nextState = c.BACKTRACK;
				return;
			}
		}
		
		/*-----------------------------------------------------
		 * Scelgo una regola fra quelle potenzialmente compatibili.
		 */
		ClauseInfo clause = clauseStore.fetch();
		
		
		/*-----------------------------------------------------
		 * Build ExecutionContext and ChoicePointContext
		 */
		ExecutionContext ec = new ExecutionContext(e.nDemoSteps++);
		ExecutionContext curCtx = e.currentContext;
		ec.clause = clause.getClause();
		//head and body with refresh variables (clause copied)
		clause.performCopy(ec.getId());
		ec.headClause = clause.getHeadCopy();
		ec.goalsToEval = new SubGoalStore();
		ec.goalsToEval.load( clause.getBodyCopy() );
		ec.choicePointAfterCut = e.choicePointSelector.getPointer();
		Struct curGoal = curCtx.currentGoal;
		List unifiedVars = (List)e.currentContext.trailingVars.getHead();
		curGoal.unify(unifiedVars,unifiedVars,ec.headClause);
		
		ec.haveAlternatives = clauseStore.haveAlternatives();
		
		//creazione cpc
		if (ec.haveAlternatives && !fromBacktracking) {
			ChoicePointContext cpc = new ChoicePointContext();
			cpc.compatibleGoals = clauseStore;
			c.saveLastTheoryStatus();
			cpc.executionContext = curCtx;
			cpc.indexSubGoal = curCtx.goalsToEval.getCurrentGoalId();
			cpc.varsToDeunify = e.currentContext.trailingVars;
			e.choicePointSelector.add(cpc);
		}
		//distruzione cpc
		if (!ec.haveAlternatives && fromBacktracking) {
			e.choicePointSelector.removeUnusedChoicePoints();
		}
		
		ec.performTailRecursionOptimization(e);
		
		ec.saveParentState();
		ec.depth = e.currentContext.depth + 1;
		e.currentContext = ec;
		e.nextState = c.GOAL_SELECTION;
	}
	
}