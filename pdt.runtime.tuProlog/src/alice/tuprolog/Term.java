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
 * Term class is the root abstract class for prolog data type
 *
 * @see Struct
 * @see Var
 * @see Number
 */
public abstract class Term implements java.io.Serializable {

	// true and false constants
	public static final Term TRUE  = new Struct("true");
	public static final Term FALSE = new Struct("false");	
	
	// checking type and properties of the Term
	
	/** is this term a prolog numeric term? */
	public abstract boolean isNumber();
	
	/** is this term a struct? */
	public abstract boolean isStruct();
	
	/** is this term a variable?  */
	public abstract boolean isVar();
	
	/** is this term a null term?*/
	public abstract boolean isNull();
	
	//
	
	/** is this term a constant prolog term? */
	public abstract boolean isAtomic();
	
	/** is this term a prolog compound term? */
	public abstract boolean isCompound();
	
	/** is this term a prolog (alphanumeric) atom? */
	public abstract boolean isAtom();
	
	/** is this term a prolog list? */
	public abstract boolean isList();
	
	/** is this term a ground term? */
	public abstract boolean isGround();
	
	/**
	 * Tests for the equality of two object terms
	 *
	 * The comparison follows the same semantic of
	 * the isEQU method.
	 *
	 */
	public boolean equals(Object t) {
		return isEqual((Term)t);
	}
	
	
	/**
	 * is term greater than term t?
	 */
	public abstract boolean isGreater(Term t);
	
	/**
	 * Tests if this term is (logically) equal to another
	 */
	public abstract boolean isEqual(Term t);
	
	/**
	 * Gets the actual term referred by this Term.
	 *
	 * if the Term is a bound variable, the method gets
	 * the Term linked to the variable
	 */
	public abstract Term getTerm();
	
	
	/**
	 * Unlink variables inside the term
	 */
	public abstract void free();
	
	
	/**
	 * Resolves variables inside the term, starting from a specific time count.
	 *
	 * If the variables has been already resolved, no renaming is done.
	 * @param count new starting time count for resolving process
	 * @return the new time count, after resolving process
	 */
	abstract long resolveTerm(long count);
	
	
	/**
	 * Resolves variables inside the term
	 * 
	 * If the variables has been already resolved, no renaming is done.
	 */
	public void resolveTerm() {
		resolveTerm(System.currentTimeMillis());
	}
	
	
	/**
	 * gets a engine's copy of this term.
	 * @param idExecCtx Execution Context identified
	 */
	public Term copyGoal(int idExecCtx, LinkedHashMap vars) {
		return copy(vars,idExecCtx);
	}
	
	
	/**
	 * gets a copy of this term for the output
	 */
	public Term copyResult(Collection goalVars, List resultVars) {
		IdentityHashMap originals = new IdentityHashMap();
		Iterator i = goalVars.iterator();
		while (i.hasNext()) {
			Var key = (Var)i.next();
			Var clone = new Var();
			try {
				clone = new Var(key.getOriginalName());
			} catch(InvalidVarNameException e) {}
			originals.put(key,clone);
			resultVars.add(clone);
		}
		return copy(originals,new IdentityHashMap());
	}
	
	
	/**
	 * gets a copy (with renamed variables) of the term.
	 *
	 * The list argument passed contains the list of variables to be renamed
	 * (if empty list then no renaming)
	 * @param idExecCtx Execution Context identifier
	 */
	abstract Term copy(AbstractMap vMap, int idExecCtx);
	
	/**
	 * gets a copy for result.
	 */
	abstract Term copy(AbstractMap vMap, AbstractMap substMap);
	
	/**
	 * Try to unify two terms
	 * @param mediator have the reference of EngineManager
	 * @param t1 the term to unify
	 * @return true if the term is unifiable with this one
	 */
	public boolean unify(Prolog mediator, Term t1) {
		EngineManager engine = mediator.getEngineManager();
		resolveTerm();
		t1.resolveTerm();
		List v1 = new ArrayList();
		List v2 = new ArrayList();
		boolean ok = unify(v1,v2,t1);
		if (ok) {
			ExecutionContext ec = engine.getCurrentContext();
			if (ec != null) {
				int id = (engine.env==null)? Var.PROGRESSIVE : engine.env.nDemoSteps;
				//Aggiorna il trailingVars
				ec.trailingVars = new OneWayList(v1,ec.trailingVars);
				//Eseguo il renaming dopo unify perchè la sua utilità non riguarda il motore ma l'utente
				int count = 0;
				Iterator it = v1.iterator();
				while (it.hasNext()) {
					((Var)it.next()).rename(id,count);
				}
				it = v2.iterator();
				while (it.hasNext()) {
					((Var)it.next()).rename(id,count);
				}
			}
			return true;
		}
		Var.free(v1);
		Var.free(v2);
		return false;
	}
	
	
	/**
	 * Tests if this term is unifiable with an other term.
	 * No unification is done.
	 *
	 * The test is done outside any demonstration context
	 * @param t the term to checked
	 *
	 * @return true if the term is unifiable with this one
	 */
	public boolean match(Term t) {
		resolveTerm();
		t.resolveTerm();
		List v1 = new ArrayList();
		List v2 = new ArrayList();
		boolean ok = unify(v1,v2,t);
		Var.free(v1);
		Var.free(v2);
		return ok;
	}
	
	
	/**
	 * Tries to unify two terms, given a demonstration context
	 * identified by the mark integer.
	 *
	 * Try the unification among the term and the term specified
	 * @param varsUnifiedArg1 Vars unified in myself
	 * @param varsUnifiedArg2 Vars unified in term t
	 */
	abstract boolean unify(List varsUnifiedArg1, List varsUnifiedArg2, Term t);
	
	
	/**
	 *
	 * Static service to parse a Term from a string.
	 * @param st the string representation of the term
	 * @return the term represented by the string
	 * @throws InvalidTermException if the string does not represent a valid term
	 *
	 */
	public static Term parse(String st) throws InvalidTermException {
		return Parser.toTerm(st);
	}
	
	/**
	 *
	 * Parse a Term from a string.
	 *
	 */
	public static Term parse(String st,OperatorManager op) throws InvalidTermException {
		return Parser.toTerm(st,op);
	}
	
	/**
	 *
	 * Parse a sentence from a string.
	 *
	 */
	static Term parseSentence(String st,OperatorManager op) throws InvalidTermException {
		try {
			Parser p=new Parser(op,st);
			if (p.readTerm(true)==Parser.TERM) {
				return p.getCurrentTerm();
			} else {
				throw new InvalidTermException();
			}
		} catch (Exception ex) {
			throw new InvalidTermException();
		}
	}
	
	/**
	 * Gets an iterator providing
	 * a term stream from a source text
	 */
	public static java.util.Iterator getIterator(String text) {
		return new TermIterator(text);
	}
	
	// term representation
	
	/**
	 * Gets the string representation of this term
	 * as an X argument of an operator, considering the associative property.
	 */
	String toStringAsArgX(OperatorManager op,int prio) {
		return toStringAsArg(op,prio,true);
	}
	
	/**
	 * Gets the string representation of this term
	 * as an Y argument of an operator, considering the associative property.
	 */
	String toStringAsArgY(OperatorManager op,int prio) {
		return toStringAsArg(op,prio,false);
	}
	
	/**
	 * Gets the string representation of this term
	 * as an argument of an operator, considering the associative property.
	 *
	 *  If the boolean argument is true, then the term must be considered
	 *  as X arg, otherwise as Y arg (referring to prolog associative rules)
	 */
	String toStringAsArg(OperatorManager op,int prio,boolean x) {
		return toString();
	}
}