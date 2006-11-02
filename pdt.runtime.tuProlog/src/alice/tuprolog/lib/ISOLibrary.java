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
 * This class represents a tuProlog library providing most of the built-ins
 * predicates and functors defined by ISO standard.
 *
 * Library/Theory dependency:  BasicLibrary
 *
 *
 *
 */
public class ISOLibrary extends Library {
	
	public ISOLibrary(){
	}
	
	public boolean atom_length_2(Term arg, Term len) {
		Struct arg0 = (Struct) arg.getTerm();
		if (!arg0.isAtom())
			return false;
		return unify(len,new Int(arg0.getName().length()));
	}
	
	
	public boolean atom_chars_2(Term arg0, Term arg1) {
		arg0 = arg0.getTerm();
		arg1 = arg1.getTerm();
		if (arg0.isVar()) {
			if (!arg1.isList()) {
				return false;
			}
			Struct list = (Struct) arg1;
			if (list.isEmptyList()) {
				return unify(arg0,new Struct(""));
			}
			String st="";
			while (!(list.isEmptyList())) {
				String st1 = list.getTerm(0).toString();
				try {
					if (st1.startsWith("'") && st1.endsWith("'")) {
						st1 = st1.substring(1,st1.length()-1);
					}
				} catch (Exception ex){};
				st = st.concat(st1);
				list = (Struct) list.getTerm(1);
			}
			return unify(arg0,new Struct(st));
		} else {
			if (!arg0.isAtom()) {
				return false;
			}
			String st = ((Struct)arg0).getName();
			Term[] tlist = new Term[st.length()];
			for (int i=0; i<st.length(); i++) {
				tlist[i] = new Struct(new String(new char[]{ st.charAt(i)} ));
			}
			Struct list = new Struct(tlist);
			/*
			 for (int i=0; i<st.length(); i++){
			 Struct ch=new Struct(new String(new char[]{ st.charAt(st.length()-i-1)} ));
			 list=new Struct( ch, list);
			 }*/
			
			return unify(arg1,list);
		}
	}
	
	
	public boolean char_code_2(Term arg0, Term arg1) {
		arg0 = arg0.getTerm();
		arg1 = arg1.getTerm();
		if (arg1.isVar()) {
			if (!arg0.isAtom()) {
				return false;
			}
			String st=((Struct)arg0).getName();
			if (st.length()>1) {
				return false;
			}
			return unify(arg1,new Int(st.charAt(0)));
		} else {
			if (!arg1.isNumber() && ((alice.tuprolog.Number)arg1).isInteger()) {
				return false;
			}
			String st = new String(new char[]{ (char)((alice.tuprolog.Number)arg1).intValue() });
			return unify(arg0,new Struct(st));
		}
	}
	
	//
	
	// functors
	
	public Term sin_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		return new alice.tuprolog.Double(Math.sin(((Number)val0).doubleValue()));
	}
	
	public Term cos_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		return new alice.tuprolog.Double(Math.cos(((Number)val0).doubleValue()));
	}
	
	public Term exp_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		return new alice.tuprolog.Double(Math.exp(((Number)val0).doubleValue()));
	}
	
	public Term atan_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		return new alice.tuprolog.Double(Math.atan(((Number)val0).doubleValue()));
	}
	
	public Term log_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		return new alice.tuprolog.Double(Math.log(((Number)val0).doubleValue()));
	}
	
	public Term sqrt_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		return new alice.tuprolog.Double(Math.sqrt(((Number)val0).doubleValue()));
	}
	
	public Term abs_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		if (((Number)val0).isInteger()) {
			return new alice.tuprolog.Int(Math.abs(((Number)val0).intValue()));
		} else {
			return new alice.tuprolog.Double(Math.abs(((Number)val0).doubleValue()));
		}
	}
	
	public Term sign_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		if (((Number)val0).isInteger()) {
			return new alice.tuprolog.Double(((Number)val0).intValue()>0 ? 1.0: -1.0);
		} else {
			return new alice.tuprolog.Double(((Number)val0).doubleValue()>0 ? 1.0: -1.0);
		}
	}
	
	public Term float_integer_part_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		return new alice.tuprolog.Double((long)Math.rint(((Number)val0).doubleValue()));
	}
	
	public Term float_fractional_part_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		double fl = ((Number)val0).doubleValue();
		return new alice.tuprolog.Double(Math.abs(fl-Math.rint(fl)));
	}
	
	public Term float_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber())
			return null;
		return new alice.tuprolog.Double(((Number) val0).doubleValue());
	}
	
	public Term floor_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		return new Int((int) Math.floor(((Number) val0).doubleValue()));
	}
	
	public Term round_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		return new alice.tuprolog.Long(Math.round(((Number)val0).doubleValue()));
	}
	
	public Term truncate_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		return new Int((int) Math.rint(((Number) val0).doubleValue()));
	}
	
	public Term ceiling_1(Term val) {
		Term val0 = evalExpression(val);
		if (!val0.isNumber()) {
			return null;
		}
		return new Int((int) Math.ceil(((Number) val0).doubleValue()));
	}
	
	public Term div_2(Term v0, Term v1) {
		Term val0 = evalExpression(v0);
		Term val1 = evalExpression(v1);
		if (!val0.isNumber()) {
			return null;
		}
		if (!val1.isNumber()) {
			return null;
		}
		return new alice.tuprolog.Int(((Number)val0).intValue()/((Number)val1).intValue());
	}
	
	public Term mod_2(Term v0, Term v1) {
		Term val0 = evalExpression(v0);
		Term val1 = evalExpression(v1);
		if (!val0.isNumber() || !val1.isNumber()) {
			return null;
		}
		int x = ((Number) val0).intValue();
        int y = ((Number) val1).intValue();
        int f = new java.lang.Double(Math.floor((double) x / (double) y)).intValue();
        return new Int(x - (f * y));
	}
	
	public Term rem_2(Term v0, Term v1) {
		Term val0 = evalExpression(v0);
		Term val1 = evalExpression(v1);
		if (!val0.isNumber()) {
			return null;
		}
		if (!val1.isNumber()) {
			return null;
		}
		return new alice.tuprolog.Double(Math.IEEEremainder(((Number)val0).doubleValue(),((Number)val1).doubleValue()));
	}
	
	/**
	 * library theory
	 */
	public String getTheory(){
		return
		//
		// operators defined by the ISOLibrary theory
		//
		":- op(  300, yfx,  'div'). \n"+
		":- op(  400, yfx,  'mod'). \n"+
		":- op(  400, yfx,  'rem'). \n"+
		":- op(  200, fx,   'sin'). \n"+
		":- op(  200, fx,   'cos'). \n"+
		":- op(  200, fx,   'sqrt'). \n"+
		":- op(  200, fx,   'atan'). \n"+
		":- op(  200, fx,   'exp'). \n"+
		":- op(  200, fx,   'log'). \n"+
		//
		// flags defined by the ISOLibrary theory
		//
		":- flag(bounded, [true,false], true, false).\n"+
		":- flag(max_integer, ["+new Integer(Integer.MAX_VALUE).toString()+"], "+new Integer(Integer.MAX_VALUE).toString()+",false).\n"+
		":- flag(min_integer, ["+new Integer(Integer.MIN_VALUE).toString()+"], "+new Integer(Integer.MIN_VALUE).toString()+",false).\n"+
		":- flag(integer_rounding_function, [up,down], down, false).\n"+
		":- flag(char_conversion,[on,off],off,false).\n"+
		":- flag(debug,[on,off],off,false).\n"+
		":- flag(max_arity, ["+new Integer(Integer.MAX_VALUE).toString()+"], "+new Integer(Integer.MAX_VALUE).toString()+",false).\n"+
		":- flag(undefined_predicate, [error,fail,warning], fail, false).\n"+
		":- flag(double_quotes, [atom,chars,codes], atom, false).\n"+
		//
		//
		"bound(X):-ground(X).\n                                                                                  "+
		"unbound(X):-not(ground(X)).\n                                                                          "+
		//
		"atom_concat(F,S,R) :- atom_chars(F,FL),atom_chars(S,SL),!,append(FL,SL,RS),atom_chars(R,RS).\n          " +
		"atom_concat(F,S,R) :- atom_chars(R,RS),append(FL,SL,RS),atom_chars(F,FL),atom_chars(S,SL).\n            " +
		"atom_codes(A,L):-atom_chars(A,L1),!,chars_codes(L1,L).\n"+
		"atom_codes(A,L):-chars_codes(L1,L),atom_chars(A,L1).\n"+
		"chars_codes([],[]).\n"+
		"chars_codes([X|L1],[Y|L2]):-char_code(X,Y),chars_codes(L1,L2).\n"+
		"sub_atom(Atom,B,L,A,Sub):-atom_chars(Atom,L1),atom_chars(Sub,L2),!,sub_list(L2,L1,B),length(L2,L), length(L1,Len), A is Len-(B+L).\n"+
		"sub_atom(Atom,B,L,A,Sub):-atom_chars(Atom,L1),sub_list(L2,L1,B),atom_chars(Sub,L2),length(L2,L), length(L1,Len), A is Len-(B+L).\n"+
		"sub_list([],_,0).\n"+
		"sub_list([X|L1],[X|L2],0):- sub_list_seq(L1,L2).\n"+
		"sub_list(L1,[_|L2],N):- sub_list(L1,L2,M), N is M + 1.\n"+
		"sub_list_seq([],L).\n"+
		"sub_list_seq([X|L1],[X|L2]):-sub_list_seq(L1,L2).\n"+
		"number_chars(Number,List):-num_atom(Number,Struct),atom_chars(Struct,List),!.\n"+
		"number_chars(Number,List):-atom_chars(Struct,List),num_atom(Number,Struct).\n"+
		"number_codes(Number,List):-num_atom(Number,Struct),atom_codes(Struct,List),!.\n"+
		"number_codes(Number,List):-atom_codes(Struct,List),num_atom(Number,Struct).\n";
		//
		// ISO default
		//"current_prolog_flag(changeable_flags,[ char_conversion(on,off), debug(on,off), undefined_predicate(error,fail,warning),double_quotes(chars,codes,atom) ]).\n"+
		//"current_prolog_flag(changeable_flags,[]).\n                                                              "+
		
	}
	
}
