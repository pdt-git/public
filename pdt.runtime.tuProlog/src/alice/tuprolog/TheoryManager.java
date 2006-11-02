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
import	java.io.*;
import  java.util.*;

import alice.tuprolog.event.WarningEvent;

/**
 * This class defines the engine clauses manager.
 * <p>
 * The clauses are stored in a Hashtable grouped by functor
 * ( each group is a list that keeps the right clause sequence)
 *
 * @see Theory
 *
 *
 *
 */
public class TheoryManager implements Serializable {
	
	/** asserted or retracted clause list */
	private   LinkedList   transientClauseList;
	
	/** register dbase changed in transientClauseList? */
	private   boolean   updateTransientClauseList;
	
	/** clause dbase */
	protected HashMap   clauseDBase;
	
	/** number of clauses */
	protected int       clauseNumber;
	
	/**  engine owner of the manager*/
	protected Prolog  engine;
	
	/** Manager of flags */
	private FlagManager flagManager;
	
	/** Manager of primitives */
	private PrimitiveManager primitiveManager;
	
	/** temporany structure for storing startGoals */
	private Stack startGoalStack;
	
	private Theory consultedTheory;
	
	
	
	public TheoryManager() {
		transientClauseList = new LinkedList();
		updateTransientClauseList = false;
		clauseDBase = new HashMap();
		clauseNumber = 0;
		try {
			consultedTheory = new Theory("");
		} catch (Exception ex) {
			/* no opration */
		}
	}
	
	/**
	 * Config this Manager
	 */
	void initialize(Prolog vm) {
		engine = vm;
		flagManager = engine.getFlagManager();
		primitiveManager = engine.getPrimitiveManager();    	
	}
	
	
	/**
	 * gets elements list related to key k
	 * @param key StructKey of predicate
	 * @return Family of kind of predicate
	 */
	private LinkedList getQueue(Object key) {
		LinkedList family = (LinkedList)clauseDBase.get(key);
		if(family == null) {
			family = new LinkedList();
			clauseDBase.put(key,family);
		}
		return(family);
	}
	
	
	private void checkPresence(Struct clause, String libName, LinkedList list) {
		Struct head = (Struct)clause.getArg(0);
		Library lib = primitiveManager.getLibraryPredicate(head.getName(),head.getArity());
		if (lib!=null) {
			if (libName!=null) {
				String msg="a builtin predicate with signature equals to the head of clause "+clause+" in library "+libName+"  has been defined in library "+
				lib.getClass().getName();
				engine.notifyWarning(new WarningEvent(engine, msg));
			} else {
				String msg="a builtin predicate with signature equals to the head of clause "+clause+" has been defined in library "+
				lib.getClass().getName();
				engine.notifyWarning(new WarningEvent(engine,msg));
			}
		} else {
			lib = primitiveManager.getLibraryFunctor(head.getName(),head.getArity());
			if (lib!=null) {
				if (libName!=null) {
					String msg = "a builtin functor with signature equals to the head of clause "+clause+" in library "+libName+"  has been defined in library "+
					lib.getClass().getName();
					engine.notifyWarning(new WarningEvent(engine,msg));
				} else {
					String msg = "a builtin functor with signature equals to the head of clause "+clause+" has been defined in library "+
					lib.getClass().getName();
					engine.notifyWarning(new WarningEvent(engine,msg));
				}
			} else {
				ClauseInfo c;
				try {
					c = (ClauseInfo)list.getFirst();
				} catch(NoSuchElementException e){
					c = null;
				}
				if (c != null) {
					String libn = c.libName;
					if (libn!=null && !libn.equals(libName)) {
						if (libName!=null) {
							String msg = "a builtin functor with signature equals to the head of clause "+clause+" has been defined in library "+
							libn;
							engine.notifyWarning(new WarningEvent(engine,msg));
						} else {
							String msg = "a clause with the same head of "+clause+"  has been defined in library "+
							libn+" as "+c.getClause();
							engine.notifyWarning(new WarningEvent(engine,msg));
						}
					}
				}
			}
		}
	}
	
	
	/**
	 * inserting of a clause at the head of the dbase
	 */
	public void assertA(Struct clause,boolean dyn,String libName,boolean backtrackable) {
		//System.out.println("ASSERTA "+clause);
		clause = toClause(clause);
		ClauseInfo d = new ClauseInfo(clause,dyn,libName);
		primitiveManager.identifyPredicate(clause);
		LinkedList family = getQueue(((Struct)d.getHead()).getHashKey());
		//
		//
		if (engine.isWarning()) {
			checkPresence(d.getClause(),libName,family);
		}
		//
		family.addFirst(d);
		if (backtrackable) {
			transOp(d,true,true);
		} else {
			d.stillValid = true;
			clauseNumber++;       
		}
	}
	
	
	/**
	 * inserting of a clause at the end of the dbase
	 */
	public void assertZ(Struct clause,boolean dyn,String libName, boolean backtrackable) {
		//System.out.println("ASSERTZ "+clause);
		clause = toClause(clause);
		ClauseInfo d = new ClauseInfo(clause,dyn,libName);
		primitiveManager.identifyPredicate(clause);
		LinkedList family = getQueue(((Struct)(d.getHead())).getHashKey());
		//
		//
		try {
			checkPresence(d.getClause(),libName,family);
		} catch(Exception e) {
			e.printStackTrace();
			checkPresence(d.getClause(),libName,family);
		}
		//
		family.addLast(d);
		if (backtrackable) {
			transOp(d,true,false);
		} else {
			d.stillValid = true;
			clauseNumber++;       
		}
	}
	
	/**
	 * removing from dbase the first clause with head unifying with clause
	 * (m if a free substitution index and t is the first free timestamp)
	 */
	public ClauseInfo retract(Struct cl) {
		//System.out.println("RETRACT "+cl);
		Struct clause = toClause(cl);
		ClauseInfo  r = null;
		LinkedList family = getQueue(((Struct)(clause.getArg(0))).getHashKey());
		Iterator it = family.iterator();
		while (it.hasNext()) {
			ClauseInfo d = (ClauseInfo)it.next();
			if (d.stillValid) {
				if (engine.match(clause,d.getClause())) {
					r = d;
					break;
				}
			}
		}
		if (r == null) {
			return null;
		}
		transOp(r,false,false);
		return new ClauseInfo(r.getClause(),true,null);
	}
	
	
	/**
	 * Returns a family of clauses with functor and arity equals
	 * to the functor and arity of the term passed as a parameter
	 */
	public LinkedList find(Term headt) {
		LinkedList r = new LinkedList();
		if (headt.isStruct()) {
			Struct head = (Struct)headt;
			LinkedList family = getQueue(head.getHashKey());//head);
			Iterator it = family.iterator();
			while (it.hasNext()) {
				ClauseInfo d = (ClauseInfo)it.next();
				if (d.stillValid) {
					//boolean f = head.unify(d.clause.getArg(0),m);
					//head.free(m);
					//d.clause.free(m);
					//if(f){
					r.addLast(new ClauseInfo(d.getClause(),d.dynamic,null));
					//}
				}
			}
		} else if (headt.isVar()) {
			Iterator it = clauseDBase.values().iterator();
			while (it.hasNext()) {
				Iterator itFamily = ((LinkedList)it.next()).iterator();
				while (itFamily.hasNext()) {
					ClauseInfo d = (ClauseInfo)itFamily.next();
					if (d.stillValid && d.dynamic) {
						r.addLast(new ClauseInfo(d.getClause(),d.dynamic,null));
					}
				}
			}
		} 
		return r;
	}
	
	
	/**
	 * Binds clauses in the database with the corresponding
	 * primitive predicate, if any
	 */
	public void rebindPrimitives() {
		//System.out.println("rebinding builtins----------------------------");
		Iterator it = clauseDBase.values().iterator();
		while (it.hasNext()) {
			Iterator itFamily = ((LinkedList) it.next()).iterator();
			while (itFamily.hasNext()) {
				ClauseInfo d = (ClauseInfo) itFamily.next();
				if (d.stillValid && d.dynamic) {
					//System.out.println("clause " + d.clause);
					Iterator itBody = d.getBody().iterator();
					while (itBody.hasNext()) {
						Term t = (Struct) itBody.next();
						primitiveManager.identifyPredicate(t);
						//System.out.println("identifying " + t);
					}
				}
			}
		}
	}
	
	
	/**
	 * returns a list of dbase valid clause copies, grouped by functor
	 */
	public LinkedList elements() {
		LinkedList r = new LinkedList();
		Iterator it = clauseDBase.values().iterator();
		while (it.hasNext()) {
			Iterator itFamily = ((LinkedList)it.next()).iterator();
			while (itFamily.hasNext()) {
				ClauseInfo d = (ClauseInfo)itFamily.next();
				if (d.stillValid) {
					r.addLast(new ClauseInfo(d.getClause(),d.dynamic,null));
				}
			}
		}
		return r;
	}
	
	
	/**
	 * returns a list of dbase valid dynamic clause
	 */
	public ArrayList getDynamicClauseList() {
		ArrayList list=new ArrayList();
		Iterator it = clauseDBase.values().iterator();
		while (it.hasNext()) {
			Iterator itFamily = ((LinkedList)it.next()).iterator();
			while (itFamily.hasNext()) {
				ClauseInfo d = (ClauseInfo)itFamily.next();
				if (d.stillValid && d.dynamic) {
					list.add(d.getClause());
				}
			}
		}
		return list;
	}
	
	/**
	 * resets dbase, clearing and and filling it with elements
	 */
	public void elements(LinkedList elements,boolean dynamicClauses,String libName) {
		clear(true);
		addElements(elements,dynamicClauses,libName);
	}
	
	/**
	 * appends a clauses list to the dbase (in the same order
	 * of elements)
	 */
	public void addElements(LinkedList elements,boolean dynamicClauses,String libName) {
		Iterator it = elements.iterator();
		while (it.hasNext()) {
			ClauseInfo d = (ClauseInfo)it.next();
			assertZ(d.getClause(),dynamicClauses,libName,true);
		}
	}
	
	/**
	 * Clears the clause dbase.
	 * 
	 * if onlyDynamic, it removes only clauses tagged 'dynamic'
	 */
	public void clear(boolean onlyDynamic) {
		if (!onlyDynamic) {
			clauseDBase = new HashMap();
			clauseNumber = 0;
		} else {
			Iterator it = clauseDBase.values().iterator();
			while (it.hasNext()) {
				Iterator itFamily = ((LinkedList)it.next()).iterator();
				while (itFamily.hasNext()) {
					ClauseInfo d = (ClauseInfo)itFamily.next();
					if (d.stillValid && d.dynamic) {
						d.stillValid = false;
						clauseNumber--;
					}
				}
			}
			optimize();
		}
		try {
			consultedTheory = new Theory("");
		} catch (Exception ex) {}
	}
	
	/**
	 * remove all the clauses of lib theory
	 */
	public void removeLibraryTheory(String libName) {
		Iterator it = clauseDBase.values().iterator();
		while (it.hasNext()) {
			Iterator itFamily = ((LinkedList)it.next()).iterator();
			while (itFamily.hasNext()) {
				ClauseInfo d = (ClauseInfo)itFamily.next();
				if (d.stillValid && d.libName!=null && libName.equals(d.libName)) {
					d.stillValid=false;
					clauseNumber--;
				}
			}
		}
		optimize();
	}
	
	/**
	 * starts a transaction on the dbase
	 */
	public void transBegin() {
		transientClauseList = new LinkedList();
		updateTransientClauseList = true;
	}
	
	/**
	 * makes a transaction operation on the dbase.
	 *
	 * d is the clause specified by the operation;
	 * if f is true then insert, else delete clause from dbase;
	 * if p is true then head insertion, else queue insertion
	 *
	 */
	public void transOp(ClauseInfo d, boolean f, boolean p) {
		if(f) {
			clauseNumber++;
			engine.spy("INSERT" + (p ? "A" : "Z") + ": " + d.getClause() + "\n");
		}
		else {
			clauseNumber--;
			engine.spy("DELETE: " + d.getClause() + "\n");
		}
		d.stillValid = f;
		if (updateTransientClauseList) {
			transientClauseList.addFirst(d);
		}
	}
	
	/**
	 * returns current liste of asserted/retracted clauses
	 */
	public List transStatus() {
		return(transientClauseList);
	}
	
	/**
	 * rollbacks dbase to a state spcified by the retracted/asserted clauses list
	 */
	public void transRestore(List ol) {
		while (!transientClauseList.equals(ol)) {
			ClauseInfo d = (ClauseInfo)transientClauseList.removeFirst();
			if (d.stillValid) {
				clauseNumber--;
				engine.spy("RESTORE-: " + d.getClause() + "\n");
			} else {
				clauseNumber++;
				engine.spy("RESTORE+: " + d.getClause() + "\n");
			}
			d.stillValid = !d.stillValid;
		}
	}
	
	/**
	 * Ends a transaction.
	 *
	 * if f is false, then rolls back the dbase to the state
	 * before transaction begin; if f is true, then commit changes
	 */
	public boolean transEnd(boolean f) {
		if (!f) {
			while (!transientClauseList.isEmpty()) {
				ClauseInfo d = (ClauseInfo)transientClauseList.removeFirst();
				if (d.stillValid) {
					clauseNumber--;
					engine.spy("RESTORE-: " + d.getClause() + "\n");
				} else {
					clauseNumber++;
					engine.spy("RESTORE+: " + d.getClause() + "\n");
				}
				d.stillValid = !d.stillValid;
			}
		}
		updateTransientClauseList = false;
		return !transientClauseList.isEmpty();
	}
	
	/**
	 * compacts dbase deleting items invalidated by retract op.
	 */
	public void optimize() {
		int n = 0;
		Iterator it=clauseDBase.values().iterator();
		while (it.hasNext()) {
			LinkedList family = (LinkedList)it.next();
			Iterator itFamily = family.iterator();
			while (itFamily.hasNext()) {
				ClauseInfo d = (ClauseInfo)itFamily.next();
				if (!d.stillValid) {
					itFamily.remove();
					n++;
				}
			}
		}
		if (n > 0) {
			engine.spy("OPTIMIZE: " + n + "\n");
		}
	}
	
	/**
	 * Consults a theory.
	 *
	 * @param theory theory to add 
	 * @param add if it is true, then adds the theory,else sets the theory.
	 * @param dynamicTheory if it is true, then the clauses are marked as dynamic
	 * @param libName if it not null, then the clauses are marked to belong to the specificed library
	 */
	public void consult(Theory theory,boolean add,boolean dynamicTheory,String libName)
	throws InvalidTheoryException {
		if (theory==null) {
			throw new InvalidTheoryException();
		}
		transBegin();
		startGoalStack = new Stack();
		LinkedList family = new LinkedList();
		boolean r=true;
		if (theory.isTextual()) {
			Parser p = new Parser(engine.getOperatorManager(),theory.toString());
			int nc = 0;
			while (p.readTerm(true) != Parser.EOF) {
				if (p.getCurrentTerm()!=null && p.getCurrentTermType()== Parser.TERM &&
						p.getCurrentTerm().isStruct()) {
					boolean result=parseClause((Struct)p.getCurrentTerm(),family,dynamicTheory,libName);
					if (!result) {
						r = false;
						break;
					}
					nc++;
					engine.spy("READ #"+nc+": " + p.getCurrentTerm());
				} else {
					r = false;
					break;
				}
			}
			if (r) {
				if(add) {
					addElements(family,dynamicTheory,libName);
				} else {
					elements(family,dynamicTheory,libName);
				}
				transEnd(true);
			} else {
				engine.spy("ERROR in " + p.getCurrentTerm());
				transEnd(false);
				throw new InvalidTheoryException(p.getCurrentLine(),p.getCurrentPos());
			}
		} else {
			try {
				Struct list = theory.getClauseListRepresentation();
				while (!list.isEmptyList()) {
					boolean result=parseClause((Struct)list.listHead(),family,dynamicTheory,libName);
					if (!result) {
						transEnd(false);
						throw new InvalidTheoryException();
					}
					list = list.listTail();
				}
				if (add) {
					addElements(family,dynamicTheory,libName);
				} else {
					elements(family,dynamicTheory,libName);
				}
				transEnd(true);
			} catch (Exception ex) {
				transEnd(false);
				throw new InvalidTheoryException();
			}
		}
		if (libName==null) {
			if (add) {
				consultedTheory = new Theory(consultedTheory.toString() + "\n" + theory.toString());
			} else {
				consultedTheory = new Theory(theory.toString());
			}
		}
	}
	
	
	private boolean parseClause(Struct co,LinkedList clauses,boolean dynamicTheory,String libName) {
		try {
			String name = co.getName();
			if ((name.equals("':-'")||name.equals(":-")) && co.getArity()==1 && co.getTerm(0).isStruct()){
				Struct dir = (Struct) co.getTerm(0);
				primitiveManager.identifyDirective(dir);
				try {
					PrimitiveInfo directive = dir.getPrimitive();
					if (directive != null)
						directive.evalAsDirective(dir);
					else {
						String msg = "The directive " + dir.getName() + "/" + dir.getArity() + " is unknown.";
						engine.notifyWarning(new WarningEvent(engine, msg));
					}
				} catch (Exception e) {
					String msg = "An exception has been thrown during the execution of the " + dir.getName() + "/" + dir.getArity() + " directive.";
					msg += "\n" + e.getMessage();
					engine.notifyWarning(new WarningEvent(engine, msg));
					// e.printStackTrace();
					// return false;
				}
				
				// directive management
				/*Struct dir=(Struct)co.getTerm(0);
				 if (dir.getName().equals("op") && dir.getArity()==3){
				 Term t0=dir.getTerm(0);
				 Term t1=dir.getTerm(1);
				 Term t2=dir.getTerm(2);
				 if (t0.isNumber() && t2.isAtom() && t1.isAtom()){
				 String st=((Struct)t1).getName();
				 if (st.equals("fx")|st.equals("fy")|st.equals("xf")|st.equals("yf")|
				 st.equals("xfx")|st.equals("yfx")|st.equals("xfy")){
				 engine.getOperatorManager().opNew(((Struct)dir.getArg(2)).getName(),
				 ((Struct)dir.getArg(1)).getName(),
				 ((Number)t0).intValue());
				 //System.out.println("new operator: "+dir);
				  }
				  }
				  } else if (dir.getName().equals("flag") && dir.getArity()==4){
				  Term flagName=dir.getTerm(0);
				  Term flagSet=dir.getTerm(1);
				  Term flagDefault=dir.getTerm(2);
				  Term flagModifiable=dir.getTerm(3);
				  if (flagSet.isList() && (flagModifiable.equals(Term.TRUE)||flagModifiable.equals(Term.FALSE))){
				  flagManager.defineFlag(flagName.toString(), (Struct)flagSet, flagDefault, flagModifiable.equals(Term.TRUE), libName);
				  }
				  } else if ((dir.getName().equals("solve")||dir.getName().equals("initialization")) && dir.getArity()==1){
				  if (dir.getTerm(0).isStruct()){
				  startGoal.push(dir.getTerm(0));
				  }
				  } else if (dir.getName().equals("load_library") && dir.getArity()==1){
				  try {
				  engine.loadLibrary(alice.util.Tools.removeApices(dir.getTerm(0).toString()));
				  } catch (Exception ex){
				  return false;
				  }
				  } else if (dir.getName().equals("consult") && dir.getArity() ==1){
				  try {
				  engine.addTheory(new Theory(
				  new FileInputStream(alice.util.Tools.removeApices(dir.getTerm(0).toString()))));
				  } catch (Exception ex){
				  return false;
				  }
				  } else {
				  // unknown directive -> consider like a simple predicate
				   //clauses.addLast(new ClauseInfo(co,dynamicTheory,libName));
				    }*/
			} else {
				co = toClause(co);
				clauses.addLast(new ClauseInfo(co,dynamicTheory,libName));
				primitiveManager.identifyPredicate(co);
			}
			return true;
		} catch (Exception ex) {
			ex.printStackTrace();
			return true;
		}
	}
	
	/**
	 * Gets a clause from a generic Term
	 */
	private Struct toClause(Struct t) {
		if (!t.isClause()) {
			t = new Struct(":-",t,new Struct("true"));
		}
		return t;
	}
	
	public void solveTheoryGoal() {
		Struct s = null;
		while (!startGoalStack.empty()) {
			s = (s == null) ?
					(Struct)startGoalStack.pop() :
						new Struct(",",(Struct)startGoalStack.pop(),s);
		}
		if (s != null) {
			try {
				engine.solve(s);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}
	
	/**
	 * add a goal eventually defined by last parsed theory.
	 */
	public void addStartGoal(Struct g) {
		startGoalStack.push(g);
	}
	
	/**
	 * saves the dbase on a output stream.
	 */
	public  boolean save(OutputStream os,boolean onlyDynamic) {
		optimize();
		boolean          r  = true;
		DataOutputStream ds = new DataOutputStream(os);
		Iterator         it = elements().iterator();
		while(it.hasNext()) {
			ClauseInfo d = (ClauseInfo)it.next();
			if (d.stillValid && !(onlyDynamic && !d.dynamic)) {
				try {
					ds.writeBytes(d.toString(engine.getOperatorManager()));
				} catch(Exception e) {
					r = false;
					break;
				}
			}
		}
		return r;
	}
	
	/**
	 * Gets current theory
	 * 
	 * @param onlyDynamic if true, fetches only dynamic clauses
	 */
	public String getTheory(boolean onlyDynamic) {
		optimize();
		StringBuffer buffer = new StringBuffer();	
		Iterator it = elements().iterator();
		while(it.hasNext()) {
			ClauseInfo d = (ClauseInfo)it.next();
			if (d.stillValid && !(onlyDynamic && !d.dynamic)) {
				try {
					buffer.append(d.toString(engine.getOperatorManager()));
					buffer.append("\n");
				} catch(Exception e) {
					return null;
				}
			}
		}
		return buffer.toString();
	}
	
	/**
	 * Gets last consulted theory
	 * 
	 * @return last theory
	 */
	public Theory getLastConsultedTheory() {
		return consultedTheory;
	}
	
	
}