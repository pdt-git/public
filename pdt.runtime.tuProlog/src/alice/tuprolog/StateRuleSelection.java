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

import java.util.List;
import java.util.ArrayList;

import alice.tuprolog.ClauseInfo;
import alice.tuprolog.Struct;
import alice.util.OneWayList;

/**
 * @author Alex Benini
 *
 */
public class StateRuleSelection extends State {
	
	
	
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
			cpc.theoryTransientClauses = c.saveLastTheoryStatus();
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