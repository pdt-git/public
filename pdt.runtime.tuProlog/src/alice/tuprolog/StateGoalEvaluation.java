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

import org.cs3.pl.tuprolog.internal.TuPrologThrowable;

/**
 * @author Alex Benini
 */
public class StateGoalEvaluation extends State {
	
	public StateGoalEvaluation(EngineManager c) {
		this.c = c;
		stateName = "Eval";
	}
	
	/* (non-Javadoc)
	 * @see alice.tuprolog.AbstractRunState#doJob()
	 */
	void doJob(Engine e) {
		if (e.currentContext.currentGoal.isPrimitive()) {
			//Recupero primitiva
			PrimitiveInfo primitive = e.currentContext.currentGoal.getPrimitive();
			try {
				e.nextState =
					(primitive.evalAsPredicate(e.currentContext.currentGoal)) ?
							c.GOAL_SELECTION : c.BACKTRACK;
			} catch (Exception ex) {
				//TODO Gestire le eccezioni in prolog
				
				/**
				 *	@author hasan abdel halim
				 *  
				 *  Introduced a new Error class in order to acheive
				 *  Exception Handling at prolog level.
			   	 * 
				 *  Ugly Solution, but it works for the moment ;).
				 */
				if ( ex.getCause() instanceof TuPrologThrowable )
					throw (TuPrologThrowable)(ex.getCause());
				
				ex.printStackTrace();
				e.nextState = c.END_HALT;
			}
			//Incremento il counter dei passi di dimostrazione
			e.nDemoSteps++;
		} else
			e.nextState = c.RULE_SELECTION;
	}
	
}