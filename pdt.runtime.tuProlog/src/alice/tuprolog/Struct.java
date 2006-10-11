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
 * Struct class represents both compound prolog term
 * and atom term (considered as 0-arity compound).
 *
 *
 *
 */
public class Struct extends Term {

    /** name of the structure */
    private String name;
    /** args array */
    private Term[] arg;
    /** arity **/
    private int arity;
    /** to speedup hash map operation */
    private StructKey hashKey;
    /** builtin info */
    private transient BuiltIn builtIn;

    private boolean resolved=false;
    private static int renameStage = 0;

    /**
     * Builds a Struct representing an atom
     */
    public Struct(String f) {
        this(f,0);
    }
    


    /**
     * Builds a compound, with one argument
     */
    public Struct(String f,Term at0) {
        this(f,1);
        arg[0] = at0;
    }

    /**
     * Builds a compound, with two arguments
     */
    public Struct(String f,Term at0,Term at1) {
        this(f,2);
        arg[0] = at0;
        arg[1] = at1;
    }

    /**
     * Builds a compound, with three arguments
     */
    public Struct(String f,Term at0,Term at1,Term at2) {
        this(f,3);
        arg[0] = at0;
        arg[1] = at1;
        arg[2] = at2;
    }

    /**
     * Builds a compound, with four arguments
     */
    public Struct(String f,Term at0,Term at1,Term at2,Term at3) {
        this(f,4);
        arg[0] = at0;
        arg[1] = at1;
        arg[2] = at2;
        arg[3] = at3;
    }

    /**
     * Builds a compound, with five arguments
     */
    public Struct(String f,Term at0,Term at1,Term at2,Term at3,Term at4) {
        this(f,5);
        arg[0] = at0;
        arg[1] = at1;
        arg[2] = at2;
        arg[3] = at3;
        arg[4] = at4;
    }

    /**
     * Builds a compound, with six arguments
     */
    public Struct(String f,Term at0,Term at1,Term at2,Term at3,Term at4,Term at5) {
        this(f,6);
        arg[0] = at0;
        arg[1] = at1;
        arg[2] = at2;
        arg[3] = at3;
        arg[4] = at4;
        arg[5] = at5;
    }

    /**
     * Builds a compound, with seven arguments
     */
    public Struct(String f,Term at0,Term at1,Term at2,Term at3,Term at4,Term at5,Term at6) {
        this(f,7);
        arg[0] = at0;
        arg[1] = at1;
        arg[2] = at2;
        arg[3] = at3;
        arg[4] = at4;
        arg[5] = at5;
        arg[6] = at6;
    }

    /**
     * Builds a compound, with an array of arguments
     */
    public Struct(String f,Term[] argList) {
        this(f,argList.length);
        for (int i=0; i<argList.length; i++){
            arg[i] = argList[i];
        }
    }



    /**
     * Builds a structure representing an empty list
     */
    public Struct() {
        this(".",2);
        arg[0] = arg[1] = NullTerm.NULL_TERM;
        resolved = true;
    }


    /**
     * Builds a list providing head and tail
     */
    public Struct(Term h,Term t) {
        this(".",2);
        arg[0] = h;
        arg[1] = t;
    }

    /**
     * Builds a list specifying the elements
     */
    public Struct(Term[] argList) {
        this(argList,0);
    }

    private Struct(Term[] argList, int index){
        this(".",2);
        if (index<argList.length){
            arg[0] = argList[index];
            arg[1] = new Struct(argList,index+1);
        } else {
            arg[0] = arg[1] = NullTerm.NULL_TERM;
        }
    }

    /**
     * Builds a compound, with a linked list of arguments
     */
    Struct(String f,alice.util.LinkedList al) {
        name = f;
        arity = al.length();
        if (arity>0){
            arg = new Term[arity];
            for(int c = 0;c < arity;c++) {
                arg[c] = (Term)al.head;
                al = al.tail;
            }
        }
        hashKey = new StructKey(name,arity);
        resolved=false;
    }

    private Struct(int arity_) {
        arity = arity_;
        arg      = new Term[arity];
    }

    private Struct(String name_,int arity_){
        name = name_;
        arity = arity_;
        if (arity>0){
            arg = new Term[arity];
        }
        hashKey = new StructKey(name,arity);
        resolved=false;
    }

    StructKey getHashKey(){
        return hashKey;
    }

    /**
     * Gets the number of elements of
     * this structure
     */
    public int getArity(){
        return arity;
    }

    /**
     * Gets the functor name  of this structure
     */
    public String getName(){
        return name;
    }

    /**
     * Gets the i-th element of this structure
     *
     * No bound check is done
     */
    public Term getArg(int index) {
        return (arg[index]);
    }

    /**
     * Gets the i-th element of this structure
     *
     * No bound check is done. It is equivalent to
     * <code>getArg(index).getTerm()</code>
     */
    public Term getTerm(int index) {
        return (arg[index].getTerm());
    }


    // checking type and properties of the Term

    /** is this term a prolog numeric term? */
    public boolean isNumber() {
        return false;
    }

    /** is this term a struct  */
    public boolean isStruct() {
        return true;
    }

    /** is this term a variable  */
    public boolean isVar() {
        return false;
    }

    public boolean isNull() {
        return false;
    }


    // check type services

    public boolean isAtomic() {
        return  (arity == 0);
    }

    public boolean isCompound() {
        return arity>0;
    }

    public boolean isAtom() {
        return (arity == 0 || isEmptyList());
    }

    public boolean isList() {
        return(name.equals(".") && arity == 2);
    }

    public boolean isGround(){
        for (int i=0; i<arity; i++){
            if (!arg[i].isGround()){
                return false;
            }
        }
        return true;
    }

    public boolean isClause() {
        return(name.equals(":-") && arity == 2 && arg[0].getTerm().isStruct());
    }

    public Term getTerm(){
        return this;
    }

    
    //
    
    /**
     * Gets an argument inside this structure, given its name
     * 
     * @param name name of the structure 
     * @return the argument or null if not found
     */
    public Struct getArg(String name){
    		if (arity==0){
    			return null;
    		}
    		for (int i=0; i<arg.length; i++){
    			if (arg[i].isStruct()){
    				Struct s = (Struct)arg[i];
    				if (s.getName().equals(name)){
    					return s;
    				}
    			}
    		}
    		for (int i=0; i<arg.length; i++){
    			if (arg[i].isStruct()){
    				Struct s = (Struct)arg[i];
    				Struct sol = s.getArg(name);
    				if (sol!=null){
    					return sol;
    				}
    			}
    		}
    		return null;
    }

    //


    public boolean isGreater(Term t) {
        t = t.getTerm();
        if (!t.isStruct()){
            return(true);
        } else if (t.isStruct()) {
            Struct ts=(Struct)t;
            int tarity= ts.arity;
            if(arity > tarity){
                return(true);
            } else if(arity == tarity) {
                if(name.compareTo(ts.name) > 0){
                    return(true);
                } else if(name.compareTo(ts.name) == 0){
                    for(int c = 0;c < arity;c++){
                        if(arg[c].isGreater(ts.arg[c])){
                            return(true);
                        } else if(arg[c].isEqual(ts.arg[c])){
                            continue;
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        return false;
    }

    public boolean isEqual(Term t) {
        t = t.getTerm();
        if (t.isStruct()){
            Struct ts=(Struct)t;
            if (arity == ts.arity && name.equals(ts.name)) {
                for (int c = 0;c < arity;c++){
                    if(!arg[c].isEqual(ts.arg[c])){
                        return(false);
                    }
                }
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    //

    /**
     * Gets a copy of this structure
     */
    public Term copy() {
        Struct t = new Struct(arity);
        t.resolved=resolved;
        t.name = name;
        t.hashKey    = hashKey;
        for(int c = 0;c < arity;c++){
            t.arg[c] = arg[c].copy();
        }
		if (builtIn!=null){
			t.builtIn = builtIn.getCopy(t);
		}
        return(t);
    }


    Term copy(alice.util.LinkedList vl) {
        Struct t = new Struct(arity);
        t.resolved=resolved;
        t.name = name;
        t.hashKey    = hashKey;
        for(int c = 0;c < arity;c++){
            t.arg[c] = arg[c].copy(vl);
        }
		if (builtIn!=null){
			t.builtIn = builtIn.getCopy(t);
		}
        return(t);
    }




    void restoreVariables(){
        for(int c = 0;c < arity;c++){
            Term t=arg[c];
            if (t.isStruct()||t.isVar()){
                t.restoreVariables();
            }
        }
    }

    /**
     * Renames variables inside the term from a specific time count.
     *
     * In order to identify if a variables has already been renamed
     * during the process, the rename-stage count is used
     *
     * @param count new starting time count for the renaming
     * @return the new time count, after the renaming
     */
    int renameVariables(int count){
        if (!resolved){
            alice.util.LinkedList vars=new alice.util.LinkedList();
            return resolveVariables(vars,count);
        } else {
            Struct.renameStage++;
            return renameVariables(Struct.renameStage,count);
        }
    }

    int renameVariables(int stage, int count){
        int newcount=count;
        for(int c = 0;c < arity;c++){
            Term t=arg[c];
            if (t.isStruct()){
                newcount=((Struct)t).renameVariables(stage,newcount);
            } else if (t.isVar()){
                newcount=((Var)t).renameVariables(stage,newcount);
            }
        }
        return newcount;
    }


    int resolveVariables(int count){
        if (resolved){
            return count;
        } else {
            alice.util.LinkedList vars=new alice.util.LinkedList();
            return resolveVariables(vars,count);
        }
    }

    /**
     * resolves time counters (which identify univocally variables)
     * from variable names.
     *
     */
     private int resolveVariables(alice.util.LinkedList vars,int count){
        int newcount=count;
        for(int c = 0;c < arity;c++){
            Term term=arg[c];
            if (term!=null){
                //--------------------------------
                // we want to resolve only not linked variables:
                // so linked variables must get the linked term
                term=term.getTerm();
                //--------------------------------
                if (term.isVar()){

                    Var t=(Var)term;
                    if (t.isAnonymous()){
                        //System.out.println("FOUND ANONYM"+t.getName());
                        t.setTime(newcount++);
                    } else {
                        // searching a variable with the same name in the list
                        String name=t.getName();
                        alice.util.LinkedList vl=vars;
                        Var found=null;
                        while(!vl.isEmptyList()) {
                            Var vn=(Var)vl.head;
                            if (name.equals(vn.getName())){
                                found=vn;
                                break;
                            } else {
                                vl = vl.tail;
                            }
                        }
                        if (found!=null) {
                            arg[c]=found;
                        } else {
                            t.setTime(newcount++);
                            vars.append(t);
                        }
                    }
                } else if (term.isStruct()){
                    newcount=((Struct)term).resolveVariables(vars,newcount);
                }
            }
        }
        resolved=true;
        return newcount;
    }


   // services for list structures

    /**
     * Is this structure an empty list?
     */
    public boolean isEmptyList() {
        return (name.equals(".") && arity == 2 && arg[0].getTerm().isNull());
    }

    /**
     * Gets the head of this structure supposed to be a list
     */
    public Term listHead(){
        return arg[0].getTerm();
    }

    /**
     * Gets the tail of this structure supposed to be  alist
     */
    public Struct listTail(){
        return ((Struct)(arg[1].getTerm()));
    }

    /**
     * Gets the number of elements of this structure supposed to be a list
     */
    public int listSize(){
        Struct t=this;
        int count=0;
        while (!t.arg[0].isNull()){
            count++;
            t=(Struct)(t.arg[1].getTerm());
        }
        return count;
    }

    /**
     * Gets an iterator on the elements of this structure supposed to be a list
     */
    public java.util.Iterator listIterator(){
        return new StructIterator(this);
    }

    // hidden services

    /**
     * Gets a list Struct representation, with the functor as first element.
     */
    Struct toList() {
        Struct t = new Struct();
        for(int c = arity - 1;c >= 0;c--){
            t = new Struct(arg[c].getTerm(),t);
        }
        return(new Struct(new Struct(name),t));
    }

    /**
     * Gets a flat Struct from this structure considered as a List
     *
     * If this structure is not a list, null object is returned
     */
    Struct fromList() {
        Term ft = arg[0].getTerm();
        if(!ft.isAtom()){
            return(null);
        }
        Struct at = (Struct)arg[1].getTerm();
        alice.util.LinkedList al = new alice.util.LinkedList();
        while (!at.isEmptyList()) {
            if(!at.isList()){
                return(null);
            }
            al.append(at.getTerm(0));
            at = (Struct)at.getTerm(1);
        }
        return(new Struct(((Struct)ft).name,al));
    }

    boolean isEmptyListRaw() {
        return (arg[0].getTerm().isNull());
    }

    /**
     * Appends an element to this structure supposed to be a list
     */
    void append(Term t) {
        if (isEmptyListRaw()) {
            arg[0] = t; arg[1] = new Struct();
        } else if(arg[1].isList()){
            ((Struct)arg[1]).append(t);
        } else {
            arg[1] = t;
        }
    }

    /**
     * Inserts (at the head) an element to this structure supposed to be a list
     */
    void insert(Term t) {
        Struct co=new Struct();
        co.arg[0]=arg[0];
        co.arg[1]=arg[1];
        arg[0] = t;
        arg[1] = co;
    }

    //

    boolean unify(Term t,int m) {
        t = t.getTerm();
        if (t.isStruct()){
            Struct ts=(Struct)t;
            if ( arity == ts.arity && name.equals(ts.name)) {
                for(int c = 0;c < arity;c++){
                    if (!arg[c].unify(ts.arg[c],m)){
                        return false;
                    }
                }
                return true;
            }
        } else if (t.isVar()){
            return t.unify(this,m);
        }
        return false;
    }

    void free(int m) {
        for(int c = 0;c < arity;c++){
            arg[c].free(m);
        }
    }

    //

    void setBuiltIn(BuiltIn b){
        builtIn=b;
    }

    BuiltIn getBuiltIn(){
        return builtIn;
    }

    boolean isBuiltIn(){
        return builtIn!=null;
    }

    //

    /**
     * Gets the string representation of this structure
     *
     * Specific representations are provided for lists and atoms.
     * Names starting with upper case letter are enclosed in apices.
     */
    public String toString() {
        // list case
        if (name.equals(".") && arity == 2){
            if (arg[0].isNull()){
                return("[]");
            } else {
                return("[" + toString0() + "]");
            }
        } else if (name.equals("{}")){
            return("{" + toString0_bracket() + "}");
        } else {
            String s = (Tokenizer.isAtom(name) ? name : "'" + name + "'");
            if (arity > 0) {
                s = s + "(";
                for(int c = 1;c < arity;c++){
                    if (!arg[c - 1].isVar()){
                        s = s + arg[c - 1].toString() + ",";
                    } else {
                        s = s + ((Var)arg[c - 1]).toStringFlattened() + ",";
                    }
                }
                if (!arg[arity - 1].isVar()){
                    s = s + arg[arity - 1].toString() + ")";
                } else {
                    s = s + ((Var)arg[arity - 1]).toStringFlattened() + ")";
                }
            }
            return(s);
        }
    }

    private String toString0() {
        Term h = arg[0].getTerm();
        Term t = arg[1].getTerm();
        if (t.isList()){
            Struct tl=(Struct)t;
            if (tl.isEmptyList()){
                return(h.toString());
            }
            if (!h.isVar()){
                return(h.toString() + "," + tl.toString0());
            } else {
                return(((Var)h).toStringFlattened() + "," + tl.toString0());
            }
        } else {
            String h0;
            String t0;
            if (h.isVar()){
                h0=((Var)h).toStringFlattened();
            } else {
                h0=h.toString();
            }
            if (t.isVar()){
                t0=((Var)t).toStringFlattened();
            } else {
                t0=t.toString();
            }
            return( h0 + "|" + t0);
        }
    }

    private String toString0_bracket() {
		if (arity==0){
			return "";
		} else if (arity==1 && !((arg[0].isStruct()) && ((Struct)arg[0]).getName().equals(","))){
        	return arg[0].getTerm().toString();
        } else {
        	// comma case 
			Term head = ((Struct)arg[0]).getTerm(0);
			Term tail = ((Struct)arg[0]).getTerm(1);
			StringBuffer buf = new StringBuffer(head.toString());
			while (tail.isStruct() && ((Struct)tail).getName().equals(",")){
				head = ((Struct)tail).getTerm(0);
				buf.append(","+head.toString());
				tail = ((Struct)tail).getTerm(1);
			}
			buf.append(","+tail.toString());
			return buf.toString();
            //	return arg[0]+","+((Struct)arg[1]).toString0_bracket();
    	}
    }

    private String toStringAsList(OperatorManager op) {
        Term h = arg[0];
        Term t = arg[1].getTerm();
        if (t.isList()){
            Struct tl=(Struct)t;
            if (tl.isEmptyList()){
                return(h.toStringAsArgY(op,0));
            }
            return(h.toStringAsArgY(op,0) + "," + tl.toStringAsList(op));
        } else {
            return(h.toStringAsArgY(op,0) + "|" + t.toStringAsArgY(op,0));
        }
    }

    String toStringAsArg(OperatorManager op,int prio,boolean x) {
        int      p = 0;
        String   v = "";

        if (name.equals(".") && arity == 2){
            if (arg[0].isNull()){
                return("[]");
            } else {
                return("[" + toStringAsList(op) + "]");
            }
        } else if (name.equals("{}")){
            return("{" + toString0_bracket() + "}");
        }

        if(arity == 2) {
            if((p = op.opPrio(name,"xfx")) >= OperatorManager.OP_LOW){
                return(
                        (((x && p >= prio) || (!x && p > prio)) ? "(" : "") +
                        arg[0].toStringAsArgX(op,p) +
                        " " + name + " "      +
                        arg[1].toStringAsArgX(op,p) +
                        (((x && p >= prio) || (!x && p > prio)) ? ")" : ""));
            }
            if((p = op.opPrio(name,"yfx")) >= OperatorManager.OP_LOW){
                return(
                        (((x && p >= prio) || (!x && p > prio)) ? "(" : "") +
                        arg[0].toStringAsArgY(op,p) +
                        " " + name + " "      +
                        arg[1].toStringAsArgX(op,p) +
                        (((x && p >= prio) || (!x && p > prio)) ? ")" : ""));
            }
            if((p = op.opPrio(name,"xfy")) >= OperatorManager.OP_LOW){
                if (!name.equals(",")){
                    return(
                            (((x && p >= prio) || (!x && p > prio)) ? "(" : "") +
                            arg[0].toStringAsArgX(op,p) +
                            " " + name + " "      +
                            arg[1].toStringAsArgY(op,p) +
                            (((x && p >= prio) || (!x && p > prio)) ? ")" : ""));
                } else {
                    return(
                            (((x && p >= prio) || (!x && p > prio)) ? "(" : "") +
                            arg[0].toStringAsArgX(op,p) +
                            //",\n\t"+
                            ","+
                            arg[1].toStringAsArgY(op,p) +
                            (((x && p >= prio) || (!x && p > prio)) ? ")" : ""));
                }
            }
        }
        else if(arity == 1) {
            if((p = op.opPrio(name,"fx")) >= OperatorManager.OP_LOW){
                return(
                        (((x && p >= prio) || (!x && p > prio)) ? "(" : "") +
                        name + " "            +
                        arg[0].toStringAsArgX(op,p) +
                        (((x && p >= prio) || (!x && p > prio)) ? ")" : ""));
            }
            if((p = op.opPrio(name,"fy")) >= OperatorManager.OP_LOW){
                return(
                    (((x && p >= prio) || (!x && p > prio)) ? "(" : "") +
                    name + " "            +
                    arg[0].toStringAsArgY(op,p) +
                    (((x && p >= prio) || (!x && p > prio)) ? ")" : ""));
            }
            if((p = op.opPrio(name,"xf")) >= OperatorManager.OP_LOW){
                return(
                    (((x && p >= prio) || (!x && p > prio)) ? "(" : "") +
                    arg[0].toStringAsArgX(op,p) +
                    " " + name + " "      +
                    (((x && p >= prio) || (!x && p > prio)) ? ")" : ""));
            }
            if((p = op.opPrio(name,"yf")) >= OperatorManager.OP_LOW){
                return(
                    (((x && p >= prio) || (!x && p > prio)) ? "(" : "") +
                    arg[0].toStringAsArgY(op,p) +
                    " " + name + " "      +
                    (((x && p >= prio) || (!x && p > prio)) ? ")" : ""));
            }
        }
        v = (Tokenizer.isAtom(name) ? name : "'" + name + "'");
        if(arity == 0){
            return(v);
        }
        v = v + "(";
        for(p = 1;p < arity;p++){
            v = v + arg[p - 1].toStringAsArgY(op,0) + ",";
        }
        v = v + arg[arity - 1].toStringAsArgY(op,0);
        v = v + ")";
        return(v);
    }

}
