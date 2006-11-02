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

/**
 * This class mantains information about a clause creation
 * (clause copy, final time T after renaming, validity stillValid Flag).
 * These information are necessary to the Theory Manager
 * to use the clause in a consistent way
 *
 */
public class ClauseInfo {
	
	/** referring clause */
	private Struct clause;
	
	/** head of clause */
	private Struct head;
	
	private Struct headCopy;
	private SubGoalTree bodyCopy;
	
	/**
	 * validity flag
	 * (if is false, then clause is not valid anymore
	 * because, for instance, it has been removed
	 * logically from theory, but phisically
	 * yet present)
	 */
	boolean stillValid;
	
	/**
	 * if dynamic clause, then  asserted/reatracted/user consulted
	 * <p>
	 * clauses from libraries are static
	 * (library clauses are removed only removing the library itself)
	 */
	boolean dynamic;
	
	/** if the clause is part of a theory in a lib (null if not)*/
	String libName;
	
	private SubGoalTree goalsTree;
	
	//usata da Find
	/**
	 * building a valid clause with a time stamp = original time stamp + NumVar in clause
	 */
	ClauseInfo(Struct clause_, boolean dyn, String lib) {
		clause = clause_;
		head = extractHead(clause);
		goalsTree = extractBody(clause.getArg(1));
		stillValid = true;
		dynamic = dyn;
		libName = lib	;
	}
	
	
	/**
	 * Gets a clause from a generic Term
	 */
	private Struct extractHead(Struct clause) {
		return (Struct)clause.getArg(0);
	}
	
	/**
	 * Gets a clause from a generic Term
	 */
	static SubGoalTree extractBody(Term body) {
		SubGoalTree r = new SubGoalTree();
		extractBody(r, body);
		return r;
	}
	
	private static void extractBody(SubGoalTree parent, Term body) {
		while (body.isStruct() && ((Struct)body).getName().equals(",")) {
			Term t = ((Struct)body).getArg(0);
			if (t.isStruct() && ((Struct)t).getName().equals(",")) {
				extractBody(parent.addChild(),t);
			} else {
				parent.addChild(t);
			}
			body = ((Struct)body).getArg(1);
		}
		parent.addChild(body);
	}
	
	
	/**
	 * Gets the string representation
	 * recognizing operators stored by
	 * the operator manager
	 */
	public String toString(OperatorManager op) {
		int p;
		if ((p = op.opPrio(":-","xfx")) >= OperatorManager.OP_LOW) {
			String st=indentPredicatesAsArgX(clause.getArg(1),op,p);
			String head = clause.getArg(0).toStringAsArgX(op,p);
			if (st.equals("true")) {
				return head +".\n";
			} else {
				return (head + " :-\n\t" + st +".\n");
			}
		}
		
		if ((p = op.opPrio(":-","yfx")) >= OperatorManager.OP_LOW) {
			String st=indentPredicatesAsArgX(clause.getArg(1),op,p);
			String head = clause.getArg(0).toStringAsArgY(op,p);
			if (st.equals("true")) {
				return head +".\n";
			} else {
				return (head + " :-\n\t" + st +".\n");
			}
		}
		
		if ((p = op.opPrio(":-","xfy")) >= OperatorManager.OP_LOW) {
			String st=indentPredicatesAsArgY(clause.getArg(1),op,p);
			String head = clause.getArg(0).toStringAsArgX(op,p);
			if (st.equals("true")) {
				return head +".\n";
			} else {
				return (head + " :-\n\t" + st +".\n");
			}
		}
		return (clause.toString());
	}
	
	Struct getClause() {
		return clause;
	}
	
	Struct getHead() {
		return head;
	}    
	
	SubGoalTree getBody() {
		return goalsTree;
	}    
	
	String getLibraryName() {
		return libName;
	}
	
	void performCopy(int idExecCtx) {
		IdentityHashMap v = new IdentityHashMap();
		headCopy = (Struct)head.copy(v,idExecCtx);
		bodyCopy = new SubGoalTree();
		bodyCopy(goalsTree,bodyCopy,v,idExecCtx);
	}
	
	private void bodyCopy(SubGoalTree source, SubGoalTree destination, AbstractMap map, int id) {
		Iterator it = source.iterator();
		while (it.hasNext()) {
			AbstractSubGoalTree s = (AbstractSubGoalTree)it.next();
			if (s.isLeaf()) {
				SubGoalElement l = (SubGoalElement)s;
				Term t = l.getValue().copy(map,id);
				destination.addChild(t);
			} else {
				SubGoalTree src  = (SubGoalTree)s; 
				SubGoalTree dest = destination.addChild();
				bodyCopy(src,dest,map,id);
			}
		}
	}
	
	Struct getHeadCopy() {
		return headCopy;
	}
	
	SubGoalTree getBodyCopy() {
		return bodyCopy;
	}
	
	
	
	/**
	 * Gets the string representation with default operator representation
	 */
	public String toString() {
		// default prio: xfx
		String st=indentPredicates(clause.getArg(1));
		return( clause.getArg(0).toString() + " :-\n\t"+st+".\n");
	}
	
	static private String indentPredicates(Term t) {
		if (t.isStruct()) {
			Struct co=(Struct)t;
			if (co.getName().equals(",")){
				return co.getArg(0).toString()+",\n\t"+indentPredicates(co.getArg(1));
			} else {
				return t.toString();
			}
		} else {
			return t.toString();
		}
	}
	
	static private String indentPredicatesAsArgX(Term t,OperatorManager op,int p) {
		if (t.isStruct()) {
			Struct co=(Struct)t;
			if (co.getName().equals(",")) {
				return co.getArg(0).toStringAsArgX(op,p)+",\n\t"+
				"("+indentPredicatesAsArgX(co.getArg(1),op,p)+")";
			} else {
				return t.toStringAsArgX(op,p);
			}
		} else {
			return t.toStringAsArgX(op,p);
		}
		
	}
	
	static private String indentPredicatesAsArgY(Term t,OperatorManager op,int p) {
		if (t.isStruct()) {
			Struct co=(Struct)t;
			if (co.getName().equals(",")) {
				return co.getArg(0).toStringAsArgY(op,p)+",\n\t"+
				"("+indentPredicatesAsArgY(co.getArg(1),op,p)+")";
			} else {
				return t.toStringAsArgY(op,p);
			}
		} else {
			return t.toStringAsArgY(op,p);
		}
	}
	
	
}