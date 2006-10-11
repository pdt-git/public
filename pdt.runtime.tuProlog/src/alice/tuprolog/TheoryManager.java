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
class TheoryManager implements Serializable {

    /** asserted or retracted clause list */
    private   alice.util.LinkedList   transientClauseList;

    /** register dbase changed in transientClauseList? */
    private   boolean   updateTransientClauseList;

    /** clause dbase */
    protected HashMap   clauseDBase;

    /** number of clauses */
    protected int       clauseNumber;

    /**  engine owner of the manager*/
    protected Prolog  engine;

    /** eventually start goal of a theory */
    private Struct startGoal;

    private Theory consultedTheory;
    
    
    public TheoryManager(Prolog vm){
        transientClauseList  = new alice.util.LinkedList();
        updateTransientClauseList  = false;
        clauseDBase  = new HashMap();
        clauseNumber  = 0;
        engine =vm;
        try {
            consultedTheory = new Theory("");
        } catch (Exception ex){
            
        }
    }

    /**
     * gets elements list related to key k
     */
    private alice.util.LinkedQueue getQueue(Object k) {
        alice.util.LinkedQueue q = (alice.util.LinkedQueue)clauseDBase.get(k);
        if(q == null) {
            q = new alice.util.LinkedQueue();
            clauseDBase.put(k,q);
        }
        
        return(q);
    }


    private void checkPresence(Struct clause, String libName, alice.util.LinkedList list){
        Struct head = (Struct)clause.getArg(0);
        Library lib = engine.getLibraryPredicate(head.getName(),head.getArity());
        if (lib!=null){
            if (libName!=null){
                engine.warn("a builtin predicate with signature equals to the head of clause "+clause+" in library "+libName+"  has been defined in library "+
                        lib.getClass().getName());
            } else {
                engine.warn("a builtin predicate with signature equals to the head of clause "+clause+" has been defined in library "+
                        lib.getClass().getName());
            }
        } else {
            lib = engine.getLibraryFunctor(head.getName(),head.getArity());
            if (lib!=null){
                if (libName!=null){
                    engine.warn("a builtin functor with signature equals to the head of clause "+clause+" in library "+libName+"  has been defined in library "+
                            lib.getClass().getName());
                } else {
                    engine.warn("a builtin functor with signature equals to the head of clause "+clause+" has been defined in library "+
                            lib.getClass().getName());
                }
            } else {
                if (list.head!=null){
                    String libn=((ClauseInfo)(list.head)).libName;
                    if (libn!=null && !libn.equals(libName)){
                        if (libName!=null){
                            engine.warn("a clause with the same head of "+clause+" in library "+libName+"  has been defined in library "+
                                    ((ClauseInfo)list.head).libName+" as "+((ClauseInfo)list.head).clause);
                        } else {
                            engine.warn("a clause with the same head of "+clause+"  has been defined in library "+
                                    ((ClauseInfo)list.head).libName+" as "+((ClauseInfo)list.head).clause);
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
        ClauseInfo  d = new ClauseInfo(clause,engine,dyn,libName);
        //System.out.println("ASSERTA "+clause);
        alice.util.LinkedQueue q = getQueue(((Struct)d.clause.getArg(0)).getHashKey());
        //
        //
        if (engine.isWarning()){
            checkPresence(d.clause,libName,q.head);
        }
        //
        q.insFirst(d);
        if (backtrackable){
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
        ClauseInfo  d = new ClauseInfo(clause,engine,dyn,libName);
        alice.util.LinkedQueue q = getQueue(((Struct)(d.clause.getArg(0))).getHashKey());
        //
        //
        checkPresence(d.clause,libName,q.head);
        //
        q.insLast(d);
        if (backtrackable){
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
    public ClauseInfo retract(Struct cl,int t,int m) {
        Struct clause     = (new ClauseInfo(cl,engine,true,null)).clause;
        ClauseInfo  r = null;
        alice.util.LinkedQueue q = getQueue(((Struct)(clause.getArg(0))).getHashKey());
        alice.util.LinkedList  l = q.head;
        while(!l.isEmptyList()) {
            ClauseInfo d = (ClauseInfo)l.head; l = l.tail;
            if(d.stillValid) {
                boolean f = clause.unify(d.clause,m);
                clause.free(m);
                d.clause.free(m);
                if(f) {
                    r = d;
                    break;
                }
            }
        }
        if(r == null){
            return(null);
        }
        transOp(r,false,false);
        return(new ClauseInfo(r.clause,t,true,null));
    }

    /**
     * finds all the clauses unifying with head and returns a list of
     * renamed (without logic names) clause copies
     */
    public alice.util.LinkedList find(Term headt,int t,int m) {
        alice.util.LinkedQueue r = new alice.util.LinkedQueue();
        
        if (headt.isStruct()){
        	    Struct head = (Struct)headt;
	        alice.util.LinkedQueue q = getQueue(head.getHashKey());//head);
	        alice.util.LinkedList  l = q.head;
	        while(!l.isEmptyList()) {
	            ClauseInfo d = (ClauseInfo)l.head;
	            l = l.tail;
	            if(d.stillValid) {
	                boolean f = head.unify(d.clause.getArg(0),m);
	                head.free(m);
	                d.clause.free(m);
	                if(f){
	                    r.insLast(new ClauseInfo(d.clause,t,d.dynamic,null));
	                }
	            }
	        }
        } else if (headt.isVar()){
        		Iterator it = clauseDBase.values().iterator();
			while (it.hasNext()){
				alice.util.LinkedList  l = ((alice.util.LinkedQueue)it.next()).head;
			    while(!l.isEmptyList()) {
			        ClauseInfo d = (ClauseInfo)l.head;
			        l = l.tail;
			        if(d.stillValid && d.dynamic) {
			        		r.insLast(new ClauseInfo(d.clause,t,d.dynamic,null));
			        }
			   }
			}
        } 
        return(r.head);
    }

    /**
     * finds all the clauses unifying with head and returns a list of
     * renamed (without logic names) clause copies
     */
    public void rebindBuiltins() {
        //System.out.println("rebinding buintins----------------------------");
        Iterator it=clauseDBase.values().iterator();
        while (it.hasNext()){
            alice.util.LinkedQueue q = (alice.util.LinkedQueue)it.next();
            alice.util.LinkedList  l = q.head;
            while(!l.isEmptyList()) {
                ClauseInfo d = (ClauseInfo)l.head;
                l = l.tail;
                if (d.stillValid && d.dynamic) {
                    //System.out.println("clause "+d.clause);
                    Term body=d.clause.getArg(1);
                    while (body.isStruct() && ((Struct)body).getName().equals(",")){
                        Term t=((Struct)body).getArg(0);
                        engine.identify(t,false);
                        //System.out.println("identifying "+t);
                        body=((Struct)body).getArg(1);
                    }
                    engine.identify(body,false);
                }
            }
        }
    }


    /**
     * returns a list of dbase valid clause copies, grouped by functor
     */
    public alice.util.LinkedList elements() {
        alice.util.LinkedQueue r = new alice.util.LinkedQueue();
        Iterator it=clauseDBase.values().iterator();
        while (it.hasNext()){
            alice.util.LinkedQueue q = (alice.util.LinkedQueue)it.next();
            alice.util.LinkedList  l = q.head;
            while(!l.isEmptyList()) {
                ClauseInfo d = (ClauseInfo)l.head; l = l.tail;
                if(d.stillValid){
                    r.insLast(new ClauseInfo(d.clause,engine,d.dynamic,null));
                }
            }
        }
        return(r.head);
    }

    /**
     * returns a list of dbase valid dynamic clause
     */
    public ArrayList getDynamicClauseList() {
        ArrayList list=new ArrayList();
        Iterator it=clauseDBase.values().iterator();
        while (it.hasNext()){
            alice.util.LinkedQueue q = (alice.util.LinkedQueue)it.next();
            alice.util.LinkedList  l = q.head;
            while(!l.isEmptyList()) {
                ClauseInfo d = (ClauseInfo)l.head;
                l = l.tail;
                if(d.stillValid && d.dynamic){
                    list.add(d.clause);
                }
            }
        }
        return list;
    }

    /**
     * resets dbase, clearing and and filling it with elements
     */
    public void elements(alice.util.LinkedList elements,boolean dynamicClauses,String libName) {
        clear(true);
        addElements(elements,dynamicClauses,libName);
    }

    /**
     * appends a clauses list to the dbase (in the same order
     * of elements)
     */
    public void addElements(alice.util.LinkedList elements,boolean dynamicClauses,String libName) {
        while(!elements.isEmptyList()) {
            ClauseInfo d = (ClauseInfo)elements.head;
            elements = elements.tail;
            assertZ(d.clause,dynamicClauses,libName,true);
        }
    }

    /**
     * Clears the clause dbase.
     * 
     * if onlyDynamic, it removes only clauses tagged 'dynamic'
     */
    public void clear(boolean onlyDynamic) {
        if (!onlyDynamic){
            clauseDBase = new HashMap();
            clauseNumber = 0;
        } else {
            Iterator it=clauseDBase.values().iterator();
            while (it.hasNext()){
                alice.util.LinkedQueue q = (alice.util.LinkedQueue)it.next();
                alice.util.LinkedList  l = q.head;
                while(!l.isEmptyList()) {
                    ClauseInfo d = (ClauseInfo)l.head;
                    l = l.tail;
                    if (d.stillValid && d.dynamic){
                        d.stillValid=false;
                        clauseNumber--;
                    }
                }
            }
            optimize();
        }
        try {
            consultedTheory = new Theory("");
        } catch (Exception ex){
        }
    }

    /**
     * remove all the clauses of lib theory
     */
    public void removeLibraryTheory(String libName) {
        Iterator it=clauseDBase.values().iterator();
        while (it.hasNext()){
            alice.util.LinkedQueue q = (alice.util.LinkedQueue)it.next();
            alice.util.LinkedList  l = q.head;
            while(!l.isEmptyList()) {
                ClauseInfo d = (ClauseInfo)l.head;
                l = l.tail;
                if (d.stillValid && d.libName!=null && libName.equals(d.libName)){
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
        transientClauseList = new alice.util.LinkedList();
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
    public void transOp(ClauseInfo d,boolean f,boolean p) {
        if(f) {
            clauseNumber++;
            engine.spy("INSERT" + (p ? "A" : "Z") + ": " + d.clause + "\n");
        }
        else {
            clauseNumber--;
            engine.spy("DELETE: " + d.clause + "\n");
        }
        d.stillValid = f;
        if(updateTransientClauseList){
            transientClauseList = new alice.util.LinkedList(d,transientClauseList);
        }
    }

    /**
     * returns current liste of asserted/retracted clauses
     */
    public alice.util.LinkedList transStatus() {
        return(transientClauseList);
    }

    /**
     * rollbacks dbase to a state spcified by the retracted/asserted clauses list
     */
    public void transRestore(alice.util.LinkedList ol) {
        while(transientClauseList != ol) {
            ClauseInfo d = (ClauseInfo)transientClauseList.head;
            transientClauseList = transientClauseList.tail;
            if(d.stillValid) {
                clauseNumber--;
                engine.spy("RESTORE-: " + d.clause + "\n");
            }
            else {
                clauseNumber++;
                engine.spy("RESTORE+: " + d.clause + "\n");
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
        if(!f) {
            while(!transientClauseList.isEmptyList()) {
                ClauseInfo d = (ClauseInfo)transientClauseList.head;
                transientClauseList = transientClauseList.tail;
                if(d.stillValid) {
                    clauseNumber--;
                    engine.spy("RESTORE-: " + d.clause + "\n");
                }
                else {
                    clauseNumber++;
                    engine.spy("RESTORE+: " + d.clause + "\n");
                }
                d.stillValid = !d.stillValid;
            }
        }
        updateTransientClauseList = false;
        return(!transientClauseList.isEmptyList());
    }

    /**
     * compacts dbase deleting items invalidated by retract op.
     */
    public void optimize() {
        int n = 0;
        Iterator it=clauseDBase.values().iterator();
        while (it.hasNext()){
            alice.util.LinkedQueue q = (alice.util.LinkedQueue)it.next();
            int      b = q.length();
            while(b-- > 0) {
                ClauseInfo d = (ClauseInfo)q.remFirst();
                if(d.stillValid){
                    q.insLast(d);
                } else {
                    n++;
                }
            }
        }
        if(n > 0){
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
        if (theory==null){
            throw new InvalidTheoryException();
        }
        transBegin();
        startGoal=null;
        alice.util.LinkedQueue        q  = new alice.util.LinkedQueue();
        Term lastTerm=null;
        boolean r=true;
        if (theory.isTextual()){
            Parser p = new Parser(engine.getOperatorManager(),theory.toString());
            int nc=0;
            while(p.readTerm(true) != Parser.EOF){
                if (p.getCurrentTerm()!=null && p.getCurrentTermType()== Parser.TERM &&
                    p.getCurrentTerm().isStruct()) {
                    boolean result=parseClause((Struct)p.getCurrentTerm(),q,dynamicTheory,libName);
                    if (!result){
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
            if (r){
                if(add){
                    addElements(q.head,dynamicTheory,libName);
                } else {
                    elements(q.head,dynamicTheory,libName);
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
                while (!list.isEmptyList()){
                    boolean result=parseClause((Struct)list.listHead(),q,dynamicTheory,libName);
                    if (!result){
                        transEnd(false);
                        throw new InvalidTheoryException();
                    }
                    list=list.listTail();
                }
                if (add){
                    addElements(q.head,dynamicTheory,libName);
                } else {
                    elements(q.head,dynamicTheory,libName);
                }
                transEnd(true);
            } catch (Exception ex){
                transEnd(false);
                throw new InvalidTheoryException();
            }
        }
        if (libName==null){
	        if (add){
	            consultedTheory = new Theory(consultedTheory.toString() + "\n" + theory.toString());
	        } else {
	            consultedTheory = new Theory(theory.toString());
	        }
        }
    }


    private boolean parseClause(Struct co,alice.util.LinkedQueue clauses,boolean dynamicTheory,String libName){
        try {
            ClauseInfo info=new ClauseInfo(co,engine,dynamicTheory,libName);
            String name=co.getName();
            if ((name.equals("':-'")||name.equals(":-")) && co.getArity()==1 && co.getTerm(0).isStruct()){
                // directive management
                Struct dir=(Struct)co.getTerm(0);
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
                        engine.defineFlag(flagName.toString(), (Struct)flagSet, flagDefault, flagModifiable.equals(Term.TRUE), libName);
                    }
                } else if ((dir.getName().equals("solve")||dir.getName().equals("initialization")) && dir.getArity()==1){
                    if (dir.getTerm(0).isStruct()){
                        startGoal=(Struct)dir.getTerm(0);
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
                	/* 
                	 * trho: 
                	 * 
                	 * @autor Tobias Rho
                	 */
                	Term[] terms = new Term[co.getArity()];
                	for (int i = 0; i < terms.length; i++) {
						terms[i] = co.getArg(i);
					}
                	if(terms.length == 1)
                		engine.solve(co.getArg(0));
                	else
                		engine.solve(new Struct(",",terms));
                    //startGoal=dir.toList();
//                    clauses.insLast(info);
                }
            } else {
                clauses.insLast(info);
            }
            return true;
        } catch (Exception ex){
            ex.printStackTrace();
            return false;
        }
    }
    /**
     * gets the goal eventually defined by last parsed theory.
     */
    public Struct getStartGoal(){
        return startGoal;
    }

    /**
     * saves the dbase on a output stream.
     */
    public  boolean save(OutputStream os,boolean onlyDynamic){
        optimize();
        boolean          r  = true;
        DataOutputStream ds = new DataOutputStream(os);
        alice.util.LinkedList          l  = elements();
        while(!l.isEmptyList()) {

            ClauseInfo d = (ClauseInfo)l.head; l = l.tail;
            if (d.stillValid && !(onlyDynamic && !d.dynamic)) {
                try {
                    ds.writeBytes(d.toString(engine.getOperatorManager()));
                } catch(Exception e) {
                    r = false;
                    break;
                }
            }
        }
        return(r);
    }

    /**
     * Gets current theory
     * 
     * @param onlyDynamic if true, fetches only dynamic clauses
     */
    public String getTheory(boolean onlyDynamic){
        optimize();
        StringBuffer buffer = new StringBuffer();	
        alice.util.LinkedList l  = elements();
        while(!l.isEmptyList()) {
            ClauseInfo d = (ClauseInfo)l.head; l = l.tail;
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
    public Theory getLastConsultedTheory(){
        return consultedTheory;
    }
}