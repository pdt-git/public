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

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.IdentityHashMap;
import java.util.List;


/**
 * Library of built-in predicates
 * @author Alex Benini
 */
public class BuiltIn extends Library {
	
	private EngineManager    engineManager;
	private TheoryManager    theoryManager;
	private LibraryManager   libraryManager;
	private FlagManager      flagManager;
	private PrimitiveManager primitiveManager;
	private OperatorManager  operatorManager;
	
	
	public BuiltIn(Prolog mediator) {
		super();
		setEngine(mediator);
		engineManager    = mediator.getEngineManager();
		theoryManager    = mediator.getTheoryManager();
		libraryManager   = mediator.getLibraryManager();
		flagManager      = mediator.getFlagManager();
		primitiveManager = mediator.getPrimitiveManager();
		operatorManager  = mediator.getOperatorManager();
	}
	
	
	/**
	 * Defines some synonyms
	 */
	public String[][] getSynonymMap(){
		return
		new String[][]{
				{"!","cut","predicate"},
				{"=","unify","predicate"},
				{"\\=","deunify","predicate"},
				{",","comma","predicate"},
				{"initialization","solve","directive"},
				{"load_library","$load_library","directive"}
		};
	}
	
	
	/*
	 * PREDICATES
	 */
	
	public boolean fail_0() {
		return false;
	}
	
	public boolean true_0() {
		return true;
	}
	
	public boolean halt_0() throws HaltException {
		throw new HaltException();
	}
	
	public boolean cut_0() {
		engineManager.cut();
		return true;
	}
	
	/**
	 * usata per la retract in basicLibrary
	 * @return
	 */
	public boolean $restore_db_0() {
		theoryManager.transRestore();
		return true;
	}
	
	
	public boolean $asserta_1(Term arg0) {
		arg0 = arg0.getTerm();
		if (!arg0.isStruct()) {
			return false;
		}
		theoryManager.assertA((Struct) arg0, true, null,false);
		return true;
	}
	
	
	public boolean $assertz_1(Term arg0) {
		arg0 = arg0.getTerm();
		if (!arg0.isStruct()) {
			return false;
		}
		theoryManager.assertZ((Struct) arg0, true, null,false);
		return true;
	}
	
	
	public boolean $retract_1(Term arg0) {
		arg0 = arg0.getTerm();
		if (!arg0.isStruct()) {
			return false;
		}
		Struct sarg0 = (Struct) arg0;
		ClauseInfo c = theoryManager.retract(sarg0);
		// if clause to retract found -> retract + true
		if (c != null) {
			Struct clause = null;
			if (!sarg0.isClause()) {
				clause = new Struct(":-", arg0, new Struct("true"));
			} else {
				clause = sarg0;
			}
			unify(clause,c.getClause());
			return true;
		}
		return false;
	}
	
	
	public boolean halt_1(Term arg0) throws HaltException {
		if (!arg0.isNumber()) {
			return false;
		} else throw new HaltException(((Number)arg0).intValue());
	}
	
	
	/*
	 * loads a tuprolog library, given its java
	 * class name
	 */
	public boolean load_library_1(Term arg0) {
		arg0 = arg0.getTerm();
		if (!arg0.isAtom()) {
			return false;
		}
		try {
			libraryManager.loadLibrary(((Struct) arg0).getName());
			return true;
		} catch (Exception ex) {
			return false;
		}
	}
	
	
	/*
	 * unloads a tuprolog library, given its java
	 * class name
	 */
	public boolean unload_library_1(Term arg0) {
		arg0 = arg0.getTerm();
		if (!arg0.isAtom()) {
			return false;
		}
		try {
			libraryManager.unloadLibrary(((Struct) arg0).getName());
			return true;
		} catch (Exception ex) {
			return false;
		}
	}
	
	
	/*
	 * get flag list:  flag_list(-List)
	 */
	public boolean flag_list_1(Term arg0) {
		arg0 = arg0.getTerm();
		Struct flist = flagManager.getPrologFlagList();
		return unify(arg0, flist);
	}
	
	
	public boolean comma_2(Term arg0, Term arg1) {
		arg0 = arg0.getTerm();
		arg1 = arg1.getTerm();
		Struct s = new Struct(",",arg0,arg1);
		engineManager.pushSubGoal(ClauseInfo.extractBody(s));
		return true;
	}
	
	
	public boolean is_2(Term arg0, Term arg1) {
		Term val1 = evalExpression(arg1);
		if (val1 == null)
			return false;
		else
			return unify(arg0.getTerm(), val1);
	}
	
	
	public boolean unify_2(Term arg0, Term arg1){
		return unify(arg0,arg1);
	}
	
	
	// \=
	public boolean deunify_2(Term arg0, Term arg1){
		return !unify(arg0, arg1);
	}	
	
	
	// $tolist
	public boolean $tolist_2(Term arg0, Term arg1){
		// transform arg0 to a list, unify it with arg1
		arg0 = arg0.getTerm();
		arg1 = arg1.getTerm();
		if (!arg0.isStruct()) {
			return false;
		}
		Term val0 = ((Struct) arg0).toList();
		return (val0 != null && unify(arg1, val0));
	}	
	
	
	// $fromlist
	public boolean $fromlist_2(Term arg0, Term arg1){
		// get the compound representation of the list
		// provided as arg1, and unify it with arg0
		arg0 = arg0.getTerm();
		arg1 = arg1.getTerm();
		if (!arg1.isList()) {
			return false;
		}
		Term val1 = ((Struct) arg1).fromList();
		return (val1 != null && unify(arg0, val1));
	}	
	
	
	// $copy
	public boolean $copy_2(Term arg0, Term arg1){
		// unify arg1 with a renamed copy of arg0
		arg0 = arg0.getTerm();
		arg1 = arg1.getTerm();
		int id = engineManager.env.nDemoSteps;
		return unify(arg1,arg0.copy(new IdentityHashMap(),id));
	}	
	
	
	// $append
	public boolean $append_2(Term arg0, Term arg1){
		// append arg0 to arg1
		arg0 = arg0.getTerm();
		arg1 = arg1.getTerm();
		if (!arg1.isList()) {
			return false;
		}
		((Struct) arg1).append(arg0);
		return true;
	}	
	
	
	// $find
	public boolean $find_2(Term arg0, Term arg1){
		// look for clauses whose head unifies whith arg0 and enqueue them to list arg1
		arg0 = arg0.getTerm();
		arg1 = arg1.getTerm();
		if (/*!arg0.isStruct() ||*/ !arg1.isList()) {
			return false;
		}
		List l = theoryManager.find(arg0);
		java.util.Iterator it = l.iterator();
		while (it.hasNext()) {
			ClauseInfo b = (ClauseInfo)it.next();
			if (match(arg0,b.getHead())) {
				b.getClause().resolveTerm();
				((Struct) arg1).append(b.getClause());
			}
		}
		return true;
	}	
	
	
	// set_prolog_flag(+Name,@Value)
	public boolean set_prolog_flag_2(Term arg0, Term arg1){
		arg0 = arg0.getTerm();
		arg1 = arg1.getTerm();
		if ( (!arg0.isAtom() && !arg0.isStruct()) || !arg1.isGround()) {
			return false;
		}
		String name = arg0.toString();
		return flagManager.setFlag(name,arg1);
	}	
	
	
	// get_prolog_flag(@Name,?Value)
	public boolean get_prolog_flag_2(Term arg0, Term arg1){
		arg0 = arg0.getTerm();
		arg1 = arg1.getTerm();
		if (!arg0.isAtom() && !arg0.isStruct()) {
			return false;
		}
		String name = arg0.toString();
		Term value = flagManager.getFlag(name);
		return (value != null) ? unify(value,arg1) : false;
	}
	
	
	// op
	public boolean $op_3(Term arg0, Term arg1, Term arg2){
		arg0 = arg0.getTerm();
		arg1 = arg1.getTerm();
		arg2 = arg2.getTerm();
		/*
		 * op(+Precedence, +Type, +Name) defines a new operator
		 * if precedence not in 0..1200 -> delete currently present op
		 */
		if (!arg0.isNumber() || !arg1.isAtom() || !arg2.isAtom()) {
			return false;
		} else {
			String st = ((Struct)arg1).getName();
			if (st.equals("fx")|st.equals("fy")|st.equals("xf")|st.equals("yf")|
					st.equals("xfx")|st.equals("yfx")|st.equals("xfy")) {
				operatorManager.opNew(
						((Struct)arg2).getName(),
						((Struct)arg1).getName(),
						((Number)arg0).intValue()
				);
				//System.out.println("new operator: "+dir);
			}
		}
		return true;
	}	
	
	
	/*
	 * DIRECTIVES
	 */
	
	public void op_3(Term arg0, Term arg1, Term arg2) {
		$op_3(arg0, arg1, arg2);
	}
	
	
	public void flag_4(Term flagName, Term flagSet, Term flagDefault, Term flagModifiable) {
		flagName       = flagName.getTerm();
		flagSet        = flagSet.getTerm();
		flagDefault    = flagDefault.getTerm();
		flagModifiable = flagModifiable.getTerm();
		if (flagSet.isList() && (flagModifiable.equals(Term.TRUE)||flagModifiable.equals(Term.FALSE))) {
			// TODO libName che futuro deve avere?? --------------------
			String libName = "";
			//------------
			flagManager.defineFlag(flagName.toString(), (Struct)flagSet, flagDefault, flagModifiable.equals(Term.TRUE), libName);
		}
	}
	
	
	public void solve_1(Term goal) {
		goal = goal.getTerm();
		if (goal.isStruct()){
			primitiveManager.identifyPredicate(goal);
			theoryManager.addStartGoal((Struct)goal);
		}		
	}
	
	
	public void $load_library_1(Term lib) throws InvalidLibraryException {
		lib = lib.getTerm();
		if (lib.isAtom()) libraryManager.loadLibrary(((Struct) lib).getName());
	}
	
	
	public void consult_1(Term theory) throws FileNotFoundException, InvalidTheoryException, IOException {
		theory = theory.getTerm();
		engine.addTheory(new Theory(
				new FileInputStream(alice.util.Tools.removeApices(theory.toString()))));
	}
	
	
}