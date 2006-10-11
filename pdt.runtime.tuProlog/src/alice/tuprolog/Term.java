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
 *
 * Term class is the root abstract class for prolog data type
 *
 * @see Struct
 * @see Var
 * @see Number
 *
 *
 *
 */
public abstract class Term implements java.io.Serializable {

    public static final Term TRUE = new Struct("true");
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
     * gets a copy of this term.
     */
    public abstract Term copy();

    /**
     * Gets a renamed copy of this term
     *
     * All the variables in the term are unlinked and renamed
     */
    public Term getRenamedCopy(){
        alice.util.LinkedList l=new alice.util.LinkedList();
        Term newterm=copy(l);
        Var.rename(l,0,true);
        return newterm;
    }

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
    abstract void free(int m);


    /**
     * Renames variables inside the term from a specific time count.
     *
     * In order to identify if a variables has already been renamed
     * during the process, the rename-stage count is used
     *
     * @param count new starting time count for the renaming
     * @return the new time count, after the renaming
     */
    abstract int renameVariables(int count);

    /**
     * Resolves variables inside the term, starting from a specific time count.
     *
     * If the variables has been already resolved, no renaming is done.
     *
     * @param count new starting time count for resolving process
     * @return the new time count, after resolving process
     */
    abstract int resolveVariables(int count);


	/**
	 * Resolves variables inside the term
	 * 
	 * If the variables has been already resolved, no renaming is done.
	 */
	public void resolveVariables(){
		resolveVariables(0);
	}


    /**
     * Restore link for variables in this term
     */
    abstract void restoreVariables();



    /**
     * gets a copy (with renamed variables) of the term.
     *
     * The list argument passed contains the list of variables to be renamed
     * (if empty list then no renaming)
     */
    abstract Term copy(alice.util.LinkedList vl);

    /**
     * Tests if this term is unifiable with an other term.
     * No unification is done.
     *
     * The test is done outside any demonstration context
     *
     * @param t the term to checked
     * @return true if the term is unifiable with this one
     */
    public boolean match(Term t){
        int c0=renameVariables(0);
        t.renameVariables(c0);
        boolean ok=unify(t,0);
        restoreVariables();
        t.restoreVariables();
        return ok;
    }


    /**
     * Tries to unify two terms
     *
     * The unification is done outside a demonstration context.
     *
     *
     * @param t the term to be unified with this one
     * @return true if the two terms have been successfully unified
     */
    public boolean unify(Term t){
        int count=renameVariables(0);
        t.renameVariables(count);
        boolean ok=unify(t,0);
        if (!ok){
            restoreVariables();
            t.restoreVariables();
        }
        return ok;
    }


    /**
     * Tries to unify two terms, given a demonstration context
     * identified by the mark integer.
     *
     * Try the unification among the term and the term specified
     * The unification is qualified with the integer passed as argument m
     * in order to make reverisble unification using free(int), passing
     * a code >= of m
     */
    abstract boolean unify(Term t,int m);

    /**
     *
     * Static service to parse a Term from a string.
     *
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
            if (p.readTerm(true)==Parser.TERM){
                return p.getCurrentTerm();
            } else {
                throw new InvalidTermException();
            }
        } catch (Exception ex){
            throw new InvalidTermException();
        }
    }

    /**
     * Gets an iterator providing
     * a term stream from a source text
     */
    public static java.util.Iterator getIterator(String text){
        return new TermIterator(text);
    }

    // term representation

    /**
    * Gets the string representation of this term
    * as an X argument of an operator, considering the associative property.
     */
    String toStringAsArgX(OperatorManager op,int prio) {
        return (toStringAsArg(op,prio,true));
    }

    /**
    * Gets the string representation of this term
    * as an Y argument of an operator, considering the associative property.
     */
    String toStringAsArgY(OperatorManager op,int prio) {
        return (toStringAsArg(op,prio,false));
    }

   /**
    * Gets the string representation of this term
    * as an argument of an operator, considering the associative property.
    *
    *  If the boolean argument is true, then the term must be considered
    *  as X arg, otherwise as Y arg (referring to prolog associative rules)
    */
    String toStringAsArg(OperatorManager op,int prio,boolean x) {
        return (toString());
    }
}