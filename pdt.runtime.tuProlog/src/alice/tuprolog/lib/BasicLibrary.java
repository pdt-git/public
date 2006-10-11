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
package alice.tuprolog.lib;
import alice.tuprolog.*;
import alice.tuprolog.Number;

/**
 * This class defines a set of basic built-in
 * predicates for the tuProlog engine
 *
 * Library/Theory dependency: none
 *
 *
 *
 */
public class BasicLibrary extends Library {

    public BasicLibrary(){
    }

    //
    // meta-predicates
    //

    /**
     * sets a new theory provided as a text
     */
    public boolean set_theory_1(Struct theory){
        try {
        	if (!theory.isAtom()){
        		return false;
        	}
            getEngine().setTheory(new Theory(theory.getName()));
            return true;
        } catch (InvalidTheoryException ex){
            System.err.println("invalid theory - line "+ex.line);
            return false;
        }
    }

    /**
     *  adds a new theory provided as a text
     */
    public boolean add_theory_1(Struct theory){
        try {
            if (!theory.isAtom()){
                return false;
            }
            getEngine().addTheory(new Theory(theory.getName()));
            return true;
        } catch (InvalidTheoryException ex){
            System.err.println("invalid theory - line "+ex.line);
            return false;
        }
    }

    /** gets current theory text */
    public boolean get_theory_1(Term arg){
        try {
            Term theory=new Struct(getEngine().getTheory().toString());
            return (unify(arg,theory));
        } catch (Exception ex){
            return false;
        }
    }

	public boolean load_library_2(Struct className, Term libName){
		try {
			Library lib = getEngine().loadLibrary(alice.util.Tools.removeApices(className.getName()));
			return unify(libName,new Struct(lib.getName()));
		} catch (Exception ex){
			return false;
		}
	}
    
    /**
     * Loads a library constructed from a theory.
     * 
     * @param theory theory text
     * @param libName name of the library
     * @return true if the library has been succesfully loaded.
     */
	public boolean load_library_from_theory_2(Struct theory, Struct libName){
        try {
            if (!theory.isAtom()){
                return false;
            }
            if (!libName.isAtom()){
                return false;
            }
            Theory th = new Theory(theory.getName());
            TheoryLibrary thlib = new TheoryLibrary(libName.getName(),th);
            getEngine().loadLibrary(thlib);
            return true;
        } catch (Exception ex){
            return false;
        }
    }
    

    public boolean get_operators_list_1(Struct arg){
        Struct list=new Struct();
        java.util.Iterator it = getEngine().getCurrentOperatorList().iterator();
        while(it.hasNext()){
            Operator o = (Operator)it.next();
            list=new Struct(new Struct("op",new alice.tuprolog.Int(o.prio),new Struct(o.type),new Struct(o.name)),list);
        }
        return (unify(arg,list));
    }

    /**
     * spawns a separate prolog agent
     * providing it a theory text
     */
    public boolean agent_1(Struct theory){
        try {
            new Agent(alice.util.Tools.removeApices(theory.toString())).spawn();
            return true;
        } catch (Exception ex){
            ex.printStackTrace();
            return false;
        }
    }

    /**
     * spawns a separate prolog agent
     * providing it a theory text and a goal
     */
    public boolean agent_2(Struct theory, Struct goal){
        try {
            new Agent(alice.util.Tools.removeApices(theory.toString()),
                goal.toString()+".").spawn();
            return true;
        } catch (Exception ex){
            ex.printStackTrace();
            return false;
        }
    }

    public boolean spy_0(){
        getEngine().setSpy(true);
        return true;
    }

    public boolean nospy_0(){
        getEngine().setSpy(false);
        return true;
    }

    public boolean warning_0(){
        getEngine().setWarning(true);
        return true;
    }

    public boolean nowarning_0(){
        getEngine().setWarning(false);
        return true;
    }

    //
    // term type inspection
    //

    public boolean constant_1(Term t){
        return (t.isAtomic());
    }

    public boolean number_1(Term t){
        return (t.isNumber());
    }

    public boolean integer_1(Number t){
        return (t.isInteger());
    }

    public boolean float_1(Number t){
        return (t.isReal());
    }

    public boolean atom_1(Term t){
        return (t.isAtom());
    }

    public boolean compound_1(Term t){
        return (t.isStruct() && !t.isAtom());
    }

    public boolean list_1(Term t){
        return (t.isList());
    }

    public boolean var_1(Term t){
        return (t.isVar());
    }

    public boolean nonvar_1(Term t){
        return (!t.isVar());
    }

    public boolean atomic_1(Term t){
        return (t.isAtom()||t.isNumber());
    }

    public boolean ground_1(Term t){
        return (t.isGround());
    }


    //
    // term/espression comparison
    //

    public boolean expression_equality_2(Term arg0, Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0==null || val1==null || !val0.isNumber() || !val1.isNumber()){
            return false;
        }
        alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
        alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
        if (val0n.isInteger() && val1n.isInteger()){
            return ((val0n.intValue()   == val1n.intValue()  ) ? true : false);
        } else {
            return((val0n.doubleValue() == val1n.doubleValue()) ? true : false);
        }
    }

    public boolean expression_greater_than_2(Term arg0, Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0==null || val1==null || !val0.isNumber() || !val1.isNumber()){
            return false;
        }
        alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
        alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
        if(val0n.isInteger() && val1n.isInteger()){
            return ((val0n.intValue()   > val1n.intValue()  ) ? true : false);
        } else {
            return((val0n.doubleValue() > val1n.doubleValue()) ? true : false);
        }
    }

    public boolean expression_less_than_2(Term arg0, Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0==null || val1==null || !val0.isNumber() || !val1.isNumber()){
            return false;
        }
        alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
        alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
        if(val0n.isInteger() && val1n.isInteger()){
            return ((val0n.intValue()   < val1n.intValue()  ) ? true : false);
        } else {
            return((val0n.doubleValue() < val1n.doubleValue()) ? true : false);
        }
    }

    public boolean term_equality_2(Term arg0, Term arg1){
        return arg0.isEqual(arg1);
    }

    public boolean term_greater_than_2(Term arg0, Term arg1){
        return arg0.isGreater(arg1);
    }

    public boolean term_less_than_2(Term arg0, Term arg1){
        return !(arg0.isGreater(arg1)||arg0.isEqual(arg1));
    }


    public Term expression_plus_1(Term arg0){
        Term val0 = evalExpression(arg0);
        if (val0!=null && val0.isNumber()){
            return(val0);
        } else {
            return null;
        }
    }

    public Term expression_minus_1(Term arg1){
        Term val0 = evalExpression(arg1);
        if (val0!=null && val0.isNumber()){
            alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
            if (val0n.isTypeInt()){
                return (new Int(val0n.intValue() * -1));
            } else if (val0n.isTypeDouble()){
                return (new alice.tuprolog.Double(val0n.doubleValue() * -1));
            } else if (val0n.isTypeLong()){
                return (new alice.tuprolog.Long(val0n.longValue() * -1));
            } else if (val0n.isTypeFloat()){
                return (new alice.tuprolog.Float(val0n.floatValue() * -1));
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    public Term expression_bitwise_not_1(Term arg0){
        Term val0 = evalExpression(arg0);
        if (val0!=null && val0.isNumber()){
            return (new Int(~((alice.tuprolog.Number)val0).intValue()));
        } else {
            return null;
        }
    }

    public Term expression_plus_2(Term arg0,Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0!=null && val1!=null && val0.isNumber() && (val1.isNumber())){
            alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
            alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
            if (val0n.isInteger() && (val1n.isInteger())){
                return(new Int(val0n.intValue() + val1n.intValue()));
            } else {
                return(new alice.tuprolog.Double(val0n.doubleValue() + val1n.doubleValue()));
            }
        } else {
            return null;
        }
}

    public Term expression_minus_2(Term arg0,Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0!=null && val1!=null && val0.isNumber() && (val1.isNumber())){
            alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
            alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
            if (val0n.isInteger() && (val1n.isInteger())){
                return(new Int(val0n.intValue() - val1n.intValue()));
            } else {
                return(new alice.tuprolog.Double(val0n.doubleValue() - val1n.doubleValue()));
            }
        } else {
            return null;
        }
    }

    public Term expression_multiply_2(Term arg0,Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0!=null && val1!=null && val0.isNumber() && (val1.isNumber())){
            alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
            alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
            if (val0n.isInteger() && (val1n.isInteger())){
                return(new Int(val0n.intValue() * val1n.intValue()));
            } else {
                return(new alice.tuprolog.Double(val0n.doubleValue() * val1n.doubleValue()));
            }
        } else {
            return null;
        }
    }

    public Term expression_div_2(Term arg0,Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0!=null && val1!=null && val0.isNumber() && (val1.isNumber())){
            alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
            alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
            return(new alice.tuprolog.Double(val0n.doubleValue()/val1n.doubleValue()));
        } else {
            return null;
        }
   }

    public Term expression_integer_div_2(Term arg0,Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0!=null && val1!=null && val0.isNumber() && (val1.isNumber())){
            alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
            alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
            return(new Int(val0n.intValue() / val1n.intValue()));
        } else {
            return null;
        }
   }
    public Term expression_pow_2(Term arg0,Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0!=null && val1!=null && val0.isNumber() && (val1.isNumber())){
            alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
            alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
            return(new alice.tuprolog.Double(Math.pow(val0n.doubleValue(),val1n.doubleValue())));
        } else {
            return null;
        }
   }

   public Term expression_bitwise_shift_right_2(Term arg0,Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0!=null && val1!=null && val0.isNumber()&&val1.isNumber()){
            alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
            alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
            return(new Int(val0n.intValue() >> val1n.intValue()));
        } else {
            return null;
        }
   }

   public Term expression_bitwise_shift_left_2(Term arg0,Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0!=null && val1!=null && val0.isNumber()&&val1.isNumber()){
            alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
            alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
            return(new Int(val0n.intValue() << val1n.intValue()));
        } else {
            return null;
        }
   }

   public Term expression_bitwise_and_2(Term arg0,Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0!=null && val1!=null && val0.isNumber()&&val1.isNumber()){
            alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
            alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
            return(new Int(val0n.intValue() & val1n.intValue()));
        } else {
            return null;
        }
   }

   public Term expression_bitwise_or_2(Term arg0,Term arg1){
        Term val0 = evalExpression(arg0);
        Term val1 = evalExpression(arg1);
        if (val0!=null && val1!=null && val0.isNumber()&&val1.isNumber()){
            alice.tuprolog.Number val0n=(alice.tuprolog.Number)val0;
            alice.tuprolog.Number val1n=(alice.tuprolog.Number)val1;
            return(new Int(val0n.intValue() | val1n.intValue()));
        } else {
            return null;
        }
   }

   //
   // text/atom manipulation predicates
   //

    /**
     * bidirectional text/term conversion.
     */
    public boolean text_term_2(Term arg0, Term arg1){
        if (!arg0.isGround()){
            return unify(arg0,new Struct(arg1.toString()));
        } else {
            try {
                String text = alice.util.Tools.removeApices(arg0.toString());
                return unify(arg1,getEngine().toTerm(text));
            } catch (Exception ex){
                return false;
            }
        }
    }


    public boolean text_concat_3(Term source1, Term source2, Term dest){
        if (source1.isAtom() && source2.isAtom()){
            return unify(dest,
                new Struct( ((Struct)source1).getName()+ ((Struct)source2).getName()));
        } else {
            return false;
        }
    }

    public boolean num_atom_2(Term arg0,Term arg1){
        if (arg1.isVar()){
            if (!arg0.isNumber()){
                return false;
            }
            alice.tuprolog.Number n0=(alice.tuprolog.Number)arg0;
            String st=null;
            if (n0.isInteger()){
                st=new java.lang.Integer(n0.intValue()).toString();
            } /*else if (arg0.isLong()){
                st=new Long(arg0.getLong()).toString();
            } else if (arg0.isFloat()){
                st=new Float(arg0.getFloat()).toString();
            } else if (arg0.isDouble()){
                st=new Double(arg0.doubleValue()).toString();
               }*/ else {
                   st=new java.lang.Double(n0.doubleValue()).toString();
               }
            return (unify(arg1,new Struct(st)));
        } else {
            if (!arg1.isAtom()){
                return false;
            }
            String st=((Struct)arg1).getName();
            try {
                if (st.startsWith("'") && st.endsWith("'")){
                    st=st.substring(1,st.length()-1);
                }
            } catch (Exception ex){};
            Term term=null;
            try {
                term=new alice.tuprolog.Int(java.lang.Integer.parseInt(st));
            } catch (Exception ex){}
            if (term==null){
                try {
                    term=new alice.tuprolog.Double(java.lang.Double.parseDouble(((Struct)arg1).getName()));
                } catch (Exception ex){}
            }
            if (term==null){
                return  false;
            }
            return (unify(arg0,term));
       }
    }


    public String getTheory(){
        return
            //
            // operators defined by the BasicLibrary theory
            //
            "':-'(op( 1200, fx,   ':-')). \n"+
            ":- op( 1200, xfx,  ':-'). \n"+
            ":- op( 1200, fx,   '?-'). \n"+
            ":- op( 1100, xfy,  ';'). \n"+
            ":- op( 1050, xfy,  '->'). \n"+
            ":- op( 1000, xfy,  ','). \n"+
            ":- op(  900, fy,   '\\+'). \n"+

            ":- op(  900, fy,   'not'). \n"+
            ":- op(  700, xfx,  '='). \n"+
            ":- op(  700, xfx,  '\\='). \n"+
            ":- op(  700, xfx,  '=='). \n"+
            ":- op(  700, xfx,  '\\=='). \n"+
            //
            ":- op(  700, xfx,  '@>'). \n"+
            ":- op(  700, xfx,  '@<'). \n"+
            ":- op(  700, xfx,  '@=<'). \n"+
            ":- op(  700, xfx,  '@>='). \n"+
            ":- op(  700, xfx,  '=:='). \n"+
            ":- op(  700, xfx,  '=\\='). \n"+
            ":- op(  700, xfx,  '>'). \n"+
            ":- op(  700, xfx,  '<'). \n"+
            ":- op(  700, xfx,  '=<'). \n"+
            ":- op(  700, xfx,  '>='). \n"+
            //
            ":- op(  700, xfx,  'is'). \n"+
            ":- op(  700, xfx,  '=..'). \n"+
            ":- op(  600, xfx,  '?'). \n"+
            ":- op(  550, xfx,  '@'). \n"+
            ":- op(  500, yfx,  '+'). \n"+
            ":- op(  500, yfx,  '-'). \n"+
            ":- op(  500, yfx,  '/\\'). \n"+
            ":- op(  500, yfx,  '\\/'). \n"+
            ":- op(  400, yfx,  '*'). \n"+
            ":- op(  400, yfx,  '/'). \n"+
            ":- op(  400, yfx,  '//'). \n"+
            ":- op(  400, yfx,  '>>'). \n"+
            ":- op(  400, yfx,  '<<'). \n"+
            ":- op(  200, xfx,  '**'). \n"+
            ":- op(  200, xfy,  '^'). \n"+
            ":- op(  200, fx,   '\\'). \n"+
            ":- op(  200, fy,   '-'). \n"+
            ":- op(  200, fy,   '+'). \n"+
            ":- op(  200, xfx,   '\\'). \n"+
            ":- op(  50, xfx,  ':'). \n"+
            //
            // flags defined by the BasicLibrary theory
            //
            ":- flag(retract_backtrackable, [true,false], true, true).\n"+
            //
            // flag management
            //
            "current_prolog_flag(Name,Value):-flag_list(L),member(flag(Name,Value),L).\n"+
            "set_prolog_flag(Name,Value):- flag(Name,Value).\n"+
            //
            // espression/term comparison
            //
            "'=:='(X,Y):- expression_equality(X,Y). \n"+
            "'=\\='(X,Y):- not expression_equality(X,Y). \n"+
            "'>'(X,Y):- expression_greater_than(X,Y). \n"+
            "'<'(X,Y):- expression_less_than(X,Y). \n"+
            "'>='(X,Y):- not expression_less_than(X,Y). \n"+
            "'=<'(X,Y):- not expression_greater_than(X,Y). \n"+
            "'=='(X,Y):- term_equality(X,Y).\n"+
            "'\\=='(X,Y):- not term_equality(X,Y).\n"+
            "'@>'(X,Y):- term_greater_than(X,Y).\n"+
            "'@<'(X,Y):- term_less_than(X,Y).\n"+
            "'@>='(X,Y):- not term_less_than(X,Y).\n"+
            "'@=<'(X,Y):- not term_greater_than(X,Y).\n"+
            //
            // meta-predicates
            //
            "'=..'(T,L)    :- nonvar(T),!,'$tolist'(T,L). \n                                                          " +
            "'=..'(T,L)    :- nonvar(L),'$fromlist'(T,L). \n                                                          " +
            "functor(T,F,A):- var(T),!,atom(F),I is A,integer(I),I>=0,newlist([],I,L),T=..[F|L]. \n                   " +
            "functor(T,F,A):- var(A),!,T=..[F|L],length(L,A). \n                                                      " +
            "functor(T,F,A):- T=..[F|L],length(L,I),I is A. \n                                                        " +
            //"arg(N,T,A)    :- nonvar(N),nonvar(T),I is N,integer(I),I>=1,T=..[F|L],element(I,L,A). \n                 " +
            "arg(N,C,T):- C =.. [_|Args], element(N,Args,T).\n"+
            "clause(H,B)   :- L=[],'$find'(H,L),member((':-'(H,B)),L). \n                                            " +
            "copy_term(T,V):-'$copy'(T,V). \n                                                                        " +
            //
            "call(P):- P. \n"+
            "'\\+'(P):- P,!,fail.\n                                                                            "+
            "'\\+'(_).\n                                                                                             "+
            "C -> T :- C,!, T; fail.\n                                                                              "+
            "A ; B :- A =.. ['->',C,T],!,(C,!,T ; B).\n                                                              "+
            "A ; B :- A. \n                                                                                          "+
            "A ; B :- B. \n                                                                                          "+
            "unify_with_occurs_check(X,Y):-X=Y.\n                                                                     "+
            "current_op(Pri,Type,Name):-get_operators_list(L),member(op(Pri,Type,Name),L).\n                          "+
            "once(X) :- myonce(X).\n                                                                                  "+
            "myonce(X):-X,!.\n                                                                                        "+
            "repeat. \n                                                                                              " +
            "repeat        :- repeat. \n                                                                             " +
            "not(G)        :- G,!,fail. \n                                                                     " +
            "not(_). \n                                                                                              " +
            //
            "bagof(T,G,B)  :- BF=[],bagof0(T,G,BF),length(BF,N),N>0,B=BF. \n                                         " +
            "bagof0(T,G,B) :- call(G),bagof1(T,B),fail. \n                                                           " +
            "bagof0(_,_,_). \n                                                                                       " +
            "bagof1(T,B)   :- '$copy'(T,V),'$append'(V,B). \n                                                        " +
            "setof(T,G,S)  :- bagof(T,G,B),quicksort(B,'@<',S). \n                                                             " +
            "findall(T,G,B)  :- BF=[],bagof0(T,G,BF),B=BF. \n                                                        " +
            //
            //
            // theory update predicates
            //
            "asserta(C)    :- '$asserta'(C). \n                                                                      " +
            "assertz(C)    :- '$assertz'(C). \n                                                                      " +
            "assert(C)     :- '$assertz'(C). \n                                                                    " +
            //
            "retract(C)    :- current_prolog_flag(retract_backtrackable,false),!,retract_nb(C).\n                        " +
            "retract(C)    :- retract_bt(C). \n                                                                 " +
            "retract_bt(C)    :- '$retract'(C). \n                                                                      " +
            "retract_bt(C)    :- '$restore_db',fail. \n                                                                 " +
            "retract_nb(C)    :- '$retract'(C). \n"+
            "retract_all([]).\n                                                                                      " +
            "retract_all([P|L]):-!,retract(P),retract_all(L).\n                                                      " +
            "abolish(P)       :- findall(P,clause(P,_),L),retract_all(L),!.\n                                            " +
            "retractall(P)    :- findall(P,clause(P,_),L),retract_all(L),!.\n                                            " +
		    //
            // auxiliary predicates
            //
            "member(E,[E|_]). \n                                                                                     " +
            "member(E,[_|L]):- member(E,L). \n                                                                       " +
            "length([],0). \n                                                                                        " +
            "length([_|L],N):- length(L,M),N is M + 1 . \n                                                           " +
            "append([],L2,L2). \n                                                                                    " +
            "append([E|T1],L2,[E|T2]):- append(T1,L2,T2). \n                                                         " +
            "reverse(L1,L2):- reverse0(L1,[],L2). \n                                                                 " +
            "reverse0([],Acc,Acc). \n                                                                                " +
            "reverse0([H|T],Acc,Y):- reverse0(T,[H|Acc],Y). \n                                                       " +
            "delete(E,[],[]). \n                                                                                     " +
            "delete(E,[E|T],L):- !,delete(E,T,L). \n                                                                 " +
            "delete(E,[H|T],[H|L]):- delete(E,T,L). \n                                                               " +
            "islist([]). \n                                                                                          " +
            "islist([_|L]):- islist(L). \n                                                                           " +
            "element(1,[E|L],E):- !. \n                                                                              " +
            "element(N,[_|L],E):- M is N - 1,element(M,L,E). \n                                                      " +
            "newlist(Ls,0,Ls):- !. \n                                                                                " +
            "newlist(Ls,N,Ld):- M is N - 1,newlist([_|Ls],M,Ld). \n                                                  " +
            "quicksort([],Pred,[]).                             \n" +
            "quicksort([X|Tail],Pred,Sorted):-                  \n"+
            "   split(X,Tail,Pred,Small,Big),                   \n"+
            "   quicksort(Small,Pred,SortedSmall),              \n"+
            "   quicksort(Big,Pred,SortedBig),                  \n"+
            "   append(SortedSmall,[X|SortedBig],Sorted).       \n"+
            "split(_,[],_,[],[]).                               \n"+
            "split(X,[Y|Tail],Pred,Small,[Y|Big]):-             \n"+
            "   Predicate =..[Pred,X,Y],                        \n"+
            "   call(Predicate),!,                              \n"+
            "   split(X,Tail,Pred,Small,Big).                   \n"+
            "split(X,[Y|Tail],Pred,[Y|Small],Big):-             \n"+
            "   split(X,Tail,Pred,Small,Big).                   \n";
    }

    /**
     * Defines some synonyms
     */
    public String[][] getOperatorSynonymMap(){
        return
            new String[][]{
                {"+","expression_plus"},
                {"-","expression_minus"},
                {"*","expression_multiply"},
                {"/","expression_div"},
                {"**","expression_pow"},
                {">>","expression_bitwise_shift_right"},
                {"<<","expression_bitwise_shift_left"},
                {"/\\","expression_bitwise_and"},
                {"\\//","expression_bitwise_or"},
                {"//","expression_integer_div"}
            };
    }

}