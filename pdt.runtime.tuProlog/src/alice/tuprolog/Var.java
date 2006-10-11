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

/**
 * This class represents a variable term.
 * Variables are identified by a name (which must starts with
 * an upper case letter) or the anonymous ('_') name.
 *
 * @see Term
 *
 *
 *
 */
public class Var extends Term {

    final static String ANY = "_";

    // the name identifying the var
    private String name;

    // time, link, mark  are used for unification process */
	private int    time;
	private int    timeBackup;
    private Term   link;
    private Term   linkBackup;
    private int    mark;
    private int    renameStage=-1;

    /**
     * Creates a variable identified by a name.
     *
     * The name must starts with an upper case letter or the underscore. If an underscore is
     * specified as a name, the variable is anonymous.
     *
     * @param n is the name
     * @throws InvalidVarNameException if n is not a valid name
     */
    public Var(String n) throws InvalidVarNameException {
        time = -1;
        link = null;
        if (n.equals(ANY)){
            name = null;
        } else if (Character.isUpperCase(n.charAt(0))||
                (n.startsWith(ANY))){
            name = n;
        } else {
            throw new InvalidVarNameException();
        }
    }

    /**
     * Creates an anonymous variable
     *
     *  This is equivalent to build a variable with name _
     */
    public Var(){
        name = null;
        time = -1;
        link = null;
    }

    //
    Var(String n,int t) {
        name = n;
        time = t;
        link = null;
    }

    /**
     * Gets the name of the variable
     */
    public String getName(){
        if (name!=null){
            return name;
        } else {
            //return ANY+time;
            return ANY;
        }
    }

    /**
     *  Gets the term which is referred by the variable.
     *
     *  For unbound variable it is the variable itself, while
     *  for bound variable it is the bound term.
     */
    public Term getTerm() {
        Term tt = this;
        Term t  = link;
        while (t != null ){
            tt = t;
            if (!t.isVar()){
                break;
            } else {
                t  = ((Var)t).link;
            }
        }
        return(tt);
    }

    //

    /** is this term a prolog numeric term? */
    public boolean isNumber() {
        return false;
    }

    /** is this term a struct  */
    public boolean isStruct() {
        return false;
    }

    /** is this term a variable  */
    public boolean isVar() {
        return true;
    }

    public boolean isNull() {
        return false;
    }


    //

    public boolean isAtomic() {
        return false;
    }

    public boolean isCompound() {
        return false;
    }

    public boolean isAtom() {
        return false;
    }

    public boolean isList() {
        return false;
    }

    public boolean isGround(){
        Term t=getTerm();
        if (t==this){
            return false;
        } else {
            return t.isGround();
        }
    }

    //

    /**
     * Tests if this variable is ANY
     */
    public boolean isAnonymous(){
        return (name==null);
    }

    /**
     * Tests if this variable is bound
     *
     */
    public boolean isBound(){
        return link!=null;
    }

    /**
     * Tests if this variable is resolved
     *
     */
    public boolean isResolved(){
        return time!=-1;
    }

    /**
     * Unlinks this variable from bound term
     */
    void unlink(){
        link=null;
    }

    void setTime(int t){
        time=t;
    }

    /**
     * finds a var Term in a list with the same time
     */
    private Var findIn(alice.util.LinkedList vl) {
        while(!vl.isEmptyList()) {
            if(time == ((Var)vl.head).time){
                return((Var)vl.head);
            }
            vl = vl.tail;
        }
        return(null);
    }


    /**
     * finds var occurence in a Struct, doing occur-check.
     */
    private boolean findIn(Struct t) {
        int arity=t.getArity();
        for(int c = 0;c < arity;c++) {
            Term at = t.getTerm(c);
            if (at.isStruct()){
                if(findIn((Struct)at)){
                    return(true);
                }
            } else if (at.isVar()){
                if(time == ((Var)at).time){
                    return(true);
                }
            }
        }
        return false;
    }

    //

    int resolveVariables(int count){
        Term tt=getTerm();
        if (tt!=this){
            return tt.resolveVariables(count);
        } else {
            return count;
        }
    }

    //

    int renameVariables(int count){
        Term tt=getTerm();
        if (tt==this){
        	timeBackup = time;
            time=count++;
            linkBackup=link;
            return count;
        } else {
            return tt.renameVariables(count);
        }
    }

    int renameVariables(int currentStage, int count){
        if (renameStage!=currentStage){
        	timeBackup = time; 
            time=count++;
            renameStage=currentStage;
            linkBackup=link;
        }
        return count;
    }

    void restoreVariables(){
        link=linkBackup;
        //time=timeBackup;
    }


    /**
     * renaming of all the variables of the argument list.
     * 
     * if f is true, the name is updated (for not anonymous var) 
     */
    static int rename(alice.util.LinkedList vl,int c,boolean f) {
        while(!vl.isEmptyList()) {
            Var v=(Var)vl.head;
            v.time = c++;
            if (f){
                if (v.name!=null){
                   v.name = ANY+v.time;
                }
            }
            vl = vl.tail;
        }
        return(c);
    }





    /**
     * var unification.
     * <p>
     * First, verify the Term eventually already unified with the same Var
     * if the Term exist, unify var with that term, in order to handle situation
     * as (A = p(X) , A = p(1)) which must produce X/1.
     * <p>
     * If instead the var is not already unified, then:
     * <p>
     * if the Term is a var bound to X, then try unification with X
     * so for example if A=1, B=A then B is unified to 1 and not to A
     * (note that it's coherent with chronological backtracking:
     * the eventually backtracked A unification is always after
     * backtracking of B unification.
     * <p>
     * if are the same Var, unification must succeed, but without any new
     * bindings (to avoid cycles for extends in A = B, B = A)
     * <p>
     * if the term is a number, then it's a success and new link is created
     * (retractable by means of a code)
     * <p>
     * if the term is a compound, then occur check test is executed:
     * the var must not appear in the compound ( avoid X=p(X),
     * or p(X,X)=p(Y,f(Y)) ); if occur check is ok
     * then it's success and a new link is created (retractable by a code)
     */
    boolean unify(Term t,int m) {
        Term tt = getTerm();
        if(tt == this) {
            t = t.getTerm();
            if (t.isVar()){
                if (time == ((Var)t).time){
                    return(true);
                }
            } else if (t.isStruct()){
                // occur-check
                if(findIn((Struct)t)){
                    return(false);
                }
            } else if (!t.isNumber()){
                return(false);
            }
            link = t;
            mark = m;
            //System.out.println("VAR "+name+" BOUND to "+link+" - time: "+time+" - mark: "+mark);
            return true;
        }
        else {
            return (tt.unify(t,m));
        }
    }

    /**
     * De-unify using the code m
     * (only link with code greater or equal than m are free)
     */
    void free(int m) {
        if(link != null) {
            link.free(m);
            if(mark >= m){
                link = null;
                //System.out.println("VAR "+name+" UNBOUND");
            }
        }
    }

    /**
     * Gets a copy of this variable
     */
    public Term copy() {
        Term tt = getTerm();
        if(tt == this) {
            return new Var(name,time);
        } else {
            return (tt.copy());
        }
    }

    /**
     * Gets a copy of this variable.
     *
     * if the variable is not present in the list passed as argument,
     * a copy of this variable is returned and added to the list. If instead
     * a variable with the same time identifier is found in the list,
     * then the variable in the list is returned.
     */
    Term copy(alice.util.LinkedList vl) {
        Term tt = getTerm();
        if(tt == this) {
            Var v = findIn(vl);
            if(v == null) {
                v = new Var(name,time);
                vl.insert(v);
            }
            return(v);
        } else {
            return(tt.copy(vl));
        }
    }

    //

    public boolean isGreater(Term t) {
        Term tt = getTerm();
        if(tt == this) {
            t = t.getTerm();
            return (t.isVar() && time > ((Var)t).time);
        }
        else {
            return (tt.isGreater(t));
        }
    }

    public boolean isEqual(Term t) {
        Term tt = getTerm();
        if(tt == this) {
            t = t.getTerm();
            return (t.isVar() && time == ((Var)t).time);
        } else {
            return (tt.isEqual(t));
        }
    }

    /**
     * Gets the string representation of this variable.
     *
     * For bounded variables, the string is <Var Name>/<bound Term>.
     */
    public String toString() {
        Term tt = getTerm();
        if (name != null){
            if (tt == this){
                return(name);
            } else {
                return(name + " / " + tt.toString());
            }
        } else {
            return ANY;
        }
        /*
        Term tt = getTerm();
        if (tt == this){
            return name;
        } else {
            return(name + " / " + tt.toString());
        }*/
    }

    /**
     * Gets the string representation of this variable, providing
     * the string representation of the linked term in the case of
     * bound variable
     *
     */
    public String toStringFlattened() {
        Term tt = getTerm();
        if (name != null){
            if(tt == this){
                return(name);
            } else {
                return(tt.toString());
            }
        } else {
            if(tt == this){
                /*
                if(time < 0){
                    return("_");
                } else {
                    return("_" + String.valueOf(time));
                }*/
                return ANY;
            } else {
                return(tt.toString());
            }
        }
        /*
        Term tt = getTerm();
        if (tt == this){
            return name;
        } else {
            return(tt.toString());
        }*/
    }
}