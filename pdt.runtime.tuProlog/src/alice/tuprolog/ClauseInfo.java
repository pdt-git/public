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

/**
 * This class mantains information about a clause creation
 * (clause copy, final time T after renaming, validity stillValid Flag).
 * These information are necessary to the Theory Manager
 * to use the clause in a consistent way
 *
 *
 *
 */
class ClauseInfo implements Serializable {

    /** referring clause */
    Struct clause;

    /** next Time Stamp after last used time in clause*/
    int timestamp;

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

    /**
     * building a valid clause with a time stamp = original time stamp + NumVar in clause
     */
    ClauseInfo(Struct clause_,int t,boolean dyn,String libName) {
        alice.util.LinkedList l = new alice.util.LinkedList();
        clause = (Struct)clause_.copy(l);
        stillValid   = true;
        timestamp = Var.rename(l,t,true);
        dynamic = dyn;
        this.libName=libName;
    }

    /**
     * building a clause ready to be saved in the dBase, rewriting
     * 'fact' clauses (i.e. p(1) becomes p(1):-True.) and assigning correct values
     * for clause Code field if the clause is a builtin predicate -
     */
    ClauseInfo(Struct clause_,Prolog engine,boolean dyn,String libName) {
        alice.util.LinkedList l = new alice.util.LinkedList();
        clause = (Struct)toClause(clause_).copy(l);
        engine.identify(clause,false);
        stillValid   = true;
        timestamp = Var.rename(l,Integer.MIN_VALUE,false);
        dynamic = dyn;
        this.libName=libName;
    }

    /**
     * Gets a clause from a generic Term
     */
    private Struct toClause(Struct t) {
        if (!t.isClause()){
            t = new Struct(":-",t,new Struct("true"));
        }
        return((Struct)t);
    }

    /**
     * Gets the string representation
     * recognizing operators stored by
     * the operator manager
     */
    public String toString(OperatorManager op) {
        int p;
        
        if ((p = op.opPrio(":-","xfx")) >= OperatorManager.OP_LOW){
            String st=indentPredicatesAsArgX(clause.getArg(1),op,p);
            String head = clause.getArg(0).toStringAsArgX(op,p);
            if (st.equals("true")){
                return head +".\n";
            } else {
                return (head + " :-\n\t" + st +".\n");
            }
        }
        
        if ((p = op.opPrio(":-","yfx")) >= OperatorManager.OP_LOW){
            String st=indentPredicatesAsArgX(clause.getArg(1),op,p);
            String head = clause.getArg(0).toStringAsArgY(op,p);
            if (st.equals("true")){
                return head +".\n";
            } else {
                return (head + " :-\n\t" + st +".\n");
            }
        }
        
        if ((p = op.opPrio(":-","xfy")) >= OperatorManager.OP_LOW){
            String st=indentPredicatesAsArgY(clause.getArg(1),op,p);
            String head = clause.getArg(0).toStringAsArgX(op,p);
            if (st.equals("true")){
                return head +".\n";
            } else {
                return (head + " :-\n\t" + st +".\n");
            }
        }
        return (clause.toString());
    }

    /**
     * Gets the string representation with default operator representation
     */
    public String toString(){
        // default prio: xfx
        String st=indentPredicates(clause.getArg(1));
        return( clause.getArg(0).toString() + " :-\n\t"+st+".\n");
    }

    static private String indentPredicates(Term t){
        if (t.isStruct()){
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

    static private String indentPredicatesAsArgX(Term t,OperatorManager op,int p){
        if (t.isStruct()){
            Struct co=(Struct)t;
            if (co.getName().equals(",")){
                return co.getArg(0).toStringAsArgX(op,p)+",\n\t"+
                		  "("+indentPredicatesAsArgX(co.getArg(1),op,p)+")";
            } else {
                return t.toStringAsArgX(op,p);
            }
        } else {
            return t.toStringAsArgX(op,p);
        }

    }

    static private String indentPredicatesAsArgY(Term t,OperatorManager op,int p){
        if (t.isStruct()){
            Struct co=(Struct)t;
            if (co.getName().equals(",")){
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