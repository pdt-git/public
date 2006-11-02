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

import java.util.*;

import alice.util.OneWayList;


/**
 * @author Alex Benini
 *
 */
public class StateBacktrack extends State {
	

	
	public StateBacktrack(EngineManager c) {
		this.c = c;
		stateName = "Back";
	}
	
	
	/* (non-Javadoc)
	 * @see alice.tuprolog.AbstractRunState#doJob()
	 */
	void doJob(Engine e) {
		ChoicePointContext curChoice = e.choicePointSelector.fetch();
		
		//verify ChoicePoint
		if (curChoice == null) {
			e.nextState = c.END_FALSE;
			return;
		}
		e.clauseSelector = curChoice.compatibleGoals;
		
		//deunify variables and reload old goal
		e.currentContext = curChoice.executionContext;
		Term curGoal = e.currentContext.goalsToEval.backTo(curChoice.indexSubGoal).getTerm();
		if (!curGoal.isStruct()) {
			e.nextState = c.END_FALSE;
			return;
		}
		e.currentContext.currentGoal = (Struct) curGoal;
		
		
		// Rende coerente l'execution_stack
		ExecutionContext curCtx = e.currentContext;
		OneWayList pointer = curCtx.trailingVars;
		OneWayList stopDeunify = curChoice.varsToDeunify;
		List varsToDeunify = (List) stopDeunify.getHead();
		Var.free(varsToDeunify);
		varsToDeunify.clear();
		SubGoalId fatherIndex;
		// bring parent contexts to a previous state in the demonstration
		do {
			// deunify variables in sibling contexts
			while (pointer != stopDeunify) {
				Var.free((List) pointer.getHead());
				pointer = pointer.getTail();
			}
			curCtx.trailingVars = pointer;
			if (curCtx.fatherCtx == null) break;
			stopDeunify = curCtx.fatherVarsList;
			fatherIndex = curCtx.fatherGoalId;
			curCtx = curCtx.fatherCtx;
			curGoal = curCtx.goalsToEval.backTo(fatherIndex).getTerm();
			if (!curGoal.isStruct()) {
				e.nextState = c.END_FALSE;
				return;
			}
			curCtx.currentGoal = (Struct)curGoal;
			pointer = curCtx.trailingVars;
		} while (true);
		
		//set next state
		e.nextState = c.GOAL_EVALUATION;
	}
	
}