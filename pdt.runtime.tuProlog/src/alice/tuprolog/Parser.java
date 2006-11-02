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
import java.io.*;

/**
 * This class defines a parser of
 * prolog terms and sentences.
 *
 *
 *
 */
public class Parser implements Serializable {
	
	public static final int TERM  = 0;
	public static final int EOF   = 1;
	public static final int ERROR = 2;
	
	/** tokenizer used by the parser */
	private Tokenizer               tokenizer;
	
	/** the term parsed */
	private Term term;
	
	/** parsing state indicator: (OK,EOF,ERROR) */
	private int type;
	
	/**
	 * the parser use a default operator manager
	 * if none is provided
	 */
	private static OperatorManager defaultOperatorManager=new DefaultOperatorManager();
	
	/** operator manager actually used */
	private OperatorManager opManager;
	
	/**
	 * creating a Parser specifing how to handle operators
	 * and what text to parse
	 */
	public Parser(OperatorManager op,String theoryText) {
		if (op!=null){
			opManager=op;
		} else {
			opManager=defaultOperatorManager;
		}
		tokenizer = new Tokenizer(new alice.util.StringInputStream(theoryText));
	}
	
	/**
	 * creating a parser with default operator interpretation
	 */
	public Parser(String theoryText) {
		this(null,theoryText);
	}
	
	// user interface
	
	/**
	 * Parses next term from the stream built on string.
	 
	 * if (endNeeded) then term+"." is required, else only term
	 */
	public int readTerm(boolean endNeeded) {
		Token tk;
		if((tk = tokenizer.readToken()).getAttribute() == Tokenizer.EOF){
			return(type = EOF);
		}
		tokenizer.unreadToken(tk);
		try {
			term    = exprA(OperatorManager.OP_HIGH);
			term.resolveTerm();
			
		} catch(Exception e) {
			//e.printStackTrace();
			term = null;
		}
		if (term == null){
			type = ERROR;
			return type;
		}
		if (endNeeded){
			if(tokenizer.readToken().getType() == Tokenizer.END){
				type = TERM;
			} else {
				type = ERROR;
			}
		} else {
			type = TERM;
		}
		return type;
	}
	
	/**
	 * Static service to get a term from its string representation
	 *
	 */
	static public Term toTerm(String st) throws InvalidTermException {
		Parser p = new Parser(st);
		Token tk;
		if ((tk = p.tokenizer.readToken()).getAttribute() == Tokenizer.EOF){
			throw new InvalidTermException();
		}
		p.tokenizer.unreadToken(tk);
		Term term=null;
		try {
			term    = p.exprA(OperatorManager.OP_HIGH);
		} catch(Exception e) {
			throw new InvalidTermException();
		}
		if (term == null){
			throw new InvalidTermException();
		} else if (p.tokenizer.readToken().getAttribute()==Tokenizer.EOF){
			try {
				term.resolveTerm();
				return term;
			} catch (Exception ex){
				throw new InvalidTermException();
			}
		} else {
			throw new InvalidTermException();
		}
	}
	
	/**
	 * Static service to get a term from its string representation,
	 * providing a specific operator manager
	 */
	static public Term toTerm(String st, OperatorManager op) throws InvalidTermException {
		Parser p = new Parser(op,st);
		Token tk;
		if ((tk = p.tokenizer.readToken()).getAttribute() == Tokenizer.EOF){
			throw new InvalidTermException();
		}
		p.tokenizer.unreadToken(tk);
		Term term=null;
		try {
			term    = p.exprA(OperatorManager.OP_HIGH);
		} catch(Exception e) {
			throw new InvalidTermException();
		}
		if (term == null){
			throw new InvalidTermException();
		} else if (p.tokenizer.readToken().getAttribute()==Tokenizer.EOF){
			try {
				term.resolveTerm();
				return term;
			} catch (Exception ex){
				throw new InvalidTermException();
			}
		} else {
			throw new InvalidTermException();
		}
	}
	
	
	public int getCurrentPos(){
		return tokenizer.getCurrentPos();
	}
	
	public int getCurrentLine(){
		return tokenizer.getCurrentLine();
	}
	
	public Term getCurrentTerm(){
		return term;
	}
	
	public int getCurrentTermType(){
		return type;
	}
	
	public Tokenizer getTokenizer(){
		return tokenizer;
	}
	
	/** goes to EOF */
	public void skip() {
		while(tokenizer.readToken().getAttribute() != Tokenizer.EOF);
	}
	
	/**
	 * refer to grammar in tuProlog documentation to understand
	 * following method
	 */
	private Term exprA(int prior) throws Exception {
		if (prior == 0){
			return(expr0());
		}
		Term tb = exprB(prior);
		if(tb != null) {
			Token tk;
			while((tk = tokenizer.readToken()).getAttribute() == Tokenizer.OPERATOR) {
				if (opManager.opPrio(tk.seq,"yfx") == prior) {
					Term ta = exprA(opManager.opNext(prior));
					if(ta != null) {
						tb = new Struct(tk.seq,tb,ta);
						continue;
					}
				}
				if(opManager.opPrio(tk.seq,"yf") == prior) {
					tb = new Struct(tk.seq,tb);
					continue;
				}
				break;
			}
			tokenizer.unreadToken(tk);
		}
		return(tb);
	}
	
	/**
	 * refer to grammar in tuProlog documentation to understand
	 * following method
	 */
	private Term exprB(int prior) throws Exception {
		Token tk;
		if((tk = tokenizer.readToken()).getAttribute() == Tokenizer.OPERATOR) {
			if(opManager.opPrio(tk.seq,"fx") == prior) {
				Term ta = exprA(opManager.opNext(prior));
				if(ta != null){
					return(new Struct(tk.seq,ta));
				}
			}
			if (opManager.opPrio(tk.seq,"fy") == prior) {
				Term ta = exprA(prior);
				if(ta != null){
					return(new Struct(tk.seq,ta));
				}
			}
		}
		tokenizer.unreadToken(tk);
		int pri=opManager.opNext(prior);
		
		Term tb = exprA(pri);
		if(tb != null) {
			if((tk = tokenizer.readToken()).getAttribute() == Tokenizer.OPERATOR) {
				if (opManager.opPrio(tk.seq,"xfx") == prior) {
					Term ta = exprA(opManager.opNext(prior));
					if(ta != null){
						return(new Struct(tk.seq,tb,ta));
					}
				}
				if (opManager.opPrio(tk.seq,"xfy") == prior) {
					Term ta = exprA(prior);
					if(ta != null){
						return(new Struct(tk.seq,tb,ta));
					}
				}
				if (opManager.opPrio(tk.seq,"xf") == prior){
					return(new Struct(tk.seq,tb));
				}
			}
			tokenizer.unreadToken(tk);
		}
		return(tb);
	}
	
	/**
	 * refer to grammar in tuProlog documentation to understand
	 * following method
	 */
	private Term expr0() throws Exception {
		Token tk   = tokenizer.readToken();
		int     type = tk.getType();
		int     attr = tk.getAttribute();
		if(type == Tokenizer.LPAR) {
			Term t = exprA(OperatorManager.OP_HIGH);
			if(tokenizer.readToken().getType() != Tokenizer.RPAR){
				throw(new Exception());
			}
			return(t);
		} else if(type == Tokenizer.LBRA) {
			if((tk = tokenizer.readToken()).getType() == Tokenizer.RBRA){
				return(new Struct());
			}
			tokenizer.unreadToken(tk);
			Term t = expr0_list();
			if(tokenizer.readToken().getType() != Tokenizer.RBRA){
				throw(new Exception());
			}
			
			return(t);
		} else if(type == Tokenizer.LBRA2) {
			//
			//
			//System.out.println("PARSING {");
			Token atok = tokenizer.readToken();
			if (atok.getType() == Tokenizer.RBRA2){
				//System.out.println("PARSING {} OK");
				return new Struct("{}"); 
			} else {
				tokenizer.unreadToken(atok);
				Term arg    = exprA(OperatorManager.OP_HIGH);
				//System.out.println("PARSING {} content: "+arg);
				if (tokenizer.readToken().getType() != Tokenizer.RBRA2){
					throw(new Exception());
				}
				Term t = new Struct("{}",arg);
				return t;
			}
			//
			//
			
			/*            
			 Term t = expr0_list_bracket();
			 if(tokenizer.readToken().getType() != Tokenizer.RBRA2){
			 throw(new Exception());
			 }
			 return(t);
			 */
		} else if(type == Tokenizer.INTEGER){
			long num=new java.lang.Long(tk.seq).longValue();
			if (num>Integer.MIN_VALUE && num<Integer.MAX_VALUE){
				return new alice.tuprolog.Int((int)num);
			} else {
				return new alice.tuprolog.Long(num);
			}
		} else if(type == Tokenizer.FLOAT){
			return (new alice.tuprolog.Double((new java.lang.Double(tk.seq)).doubleValue()));
		} else if(type == Tokenizer.VARIABLE) {
			//-- RICCI 020906
			/*
			 Term t = findIn(tk.seq,varList);
			 if(tk.seq.equals("_") || t == null) {
			 t       = new Var(tk.seq, varList.length() + 1);
			 varList = new alice.util.LinkedList(t,varList);
			 }
			 return(t);
			 */
			if (tk.seq.equals("_")){
				return new Var();
			} else {
				return new Var(tk.seq);
			}
			//---RICCI 020906
			
		} else if(type == Tokenizer.ATOM || type == Tokenizer.SQ_SEQUENCE){
			if(attr != Tokenizer.FUNCTOR){
				return(new Struct(tk.seq));
			} else {
				String  f = tk.seq;
				// reading open par
				Token tok=tokenizer.readToken();
				
				alice.util.LinkedList a = expr0_arglist();
				if(tokenizer.readToken().getType() != Tokenizer.RPAR){
					throw(new Exception());
				}
				Struct newCo=new Struct(f,a);
				//System.out.println("NEW COMPOUND "+newCo);
				return(newCo);
			}
		}
		throw(new Exception());
	}
	
	private Term expr0_list() throws Exception {
		Term head = exprA_inStruct(OperatorManager.OP_HIGH);
		Token tok=tokenizer.readToken();
		if (tok.seq.equals(",")){
			return new Struct(head,expr0_list());
		}
		if (tok.seq.equals("|")){
			return new Struct(head,exprA_inStruct(OperatorManager.OP_HIGH));
		}
		if (tok.seq.equals("]")){
			tokenizer.unreadToken(tok);
			return new Struct(head,new Struct());
		}
		throw(new Exception());
	}
	
	
	// RICCI 030227 begin
	
	// original
	/*
	 
	 private Term expr0_list_bracket() throws Exception {
	 Term head = exprA_inStruct(OperatorManager.OP_HIGH);
	 Token tok=tokenizer.readToken();
	 if (tok.seq.equals(",")){
	 return new Struct("{}",head,expr0_list_bracket());
	 }
	 if (tok.seq.equals("}")){
	 tokenizer.unreadToken(tok);
	 return new Struct("{}",head);
	 }
	 throw(new Exception());
	 }*/
	
	// new one
	
	private Term expr0_list_bracket() throws Exception {
		Term head = exprA_inStruct(OperatorManager.OP_HIGH);
		Token tok=tokenizer.readToken();
		if (tok.seq.equals(",")){
			return new Struct("{}",new Struct(",",head,expr0_list_bracket2()));
		}
		if (tok.seq.equals("}")){
			tokenizer.unreadToken(tok);
			return new Struct("{}",head);
		}
		throw(new Exception());
	}
	
	private Term expr0_list_bracket2() throws Exception {
		Term head = exprA_inStruct(OperatorManager.OP_HIGH);
		Token tok=tokenizer.readToken();
		if (tok.seq.equals(",")){
			return new Struct(",",head,expr0_list_bracket2());
		}
		if (tok.seq.equals("}")){
			tokenizer.unreadToken(tok);
			return head;
		}
		throw(new Exception());
	}
	
	
	// RICCI 030227 end
	
	private alice.util.LinkedList expr0_arglist() throws Exception {
		Term head = exprA_inStruct(OperatorManager.OP_HIGH);
		Token tok=tokenizer.readToken();
		if (tok.seq.equals(",")){
			return(new alice.util.LinkedList(head,expr0_arglist()));
		}
		if (tok.seq.equals(")")){
			tokenizer.unreadToken(tok);
			return(new alice.util.LinkedList(head,new alice.util.LinkedList()));
		}
		throw(new Exception());
	}
	
	
	/*
	 following two methods mirror exprA and exprB, but
	 for parsing argument inside compound structure ->
	 the difference is here the treatment of the COMMA:
	 in following methods comma is NOT CONSIDERED as OPERATOR,
	 but as a builtin parsing symbol, separating
	 structure arguments
	 */
	
	/**
	 * refer to grammar in tuProlog documentation to understand
	 * following method
	 */
	private Term exprA_inStruct(int prior) throws Exception {
		if (prior == 0){
			return(expr0());
		}
		Term tb = exprB_inStruct(prior);
		if(tb != null) {
			Token tk;
			while((tk = tokenizer.readToken()).getAttribute() == Tokenizer.OPERATOR) {
				if (tk.seq.equals(",")){
					break;
				}
				if(opManager.opPrio(tk.seq,"yfx") == prior) {
					Term ta = exprA_inStruct(opManager.opNext(prior));
					if(ta != null) {
						tb = new Struct(tk.seq,tb,ta);
						continue;
					}
				}
				if(opManager.opPrio(tk.seq,"yf") == prior) {
					tb = new Struct(tk.seq,tb);
					continue;
				}
				break;
			}
			tokenizer.unreadToken(tk);
		}
		return(tb);
	}
	
	/**
	 * refer to grammar in tuProlog documentation to understand
	 * following method
	 */
	private Term exprB_inStruct(int prior) throws Exception {
		Token tk;
		if ((tk = tokenizer.readToken()).getAttribute() == Tokenizer.OPERATOR
				&& !tk.seq.equals(",")) {
			if(opManager.opPrio(tk.seq,"fx") == prior) {
				Term ta = exprA_inStruct(opManager.opNext(prior));
				if(ta != null){
					return(new Struct(tk.seq,ta));
				}
			}
			if (opManager.opPrio(tk.seq,"fy") == prior) {
				Term ta = exprA_inStruct(prior);
				if(ta != null){
					return(new Struct(tk.seq,ta));
				}
			}
		}
		tokenizer.unreadToken(tk);
		int pri=opManager.opNext(prior);
		Term tb = exprA_inStruct(pri);
		if(tb != null) {
			if ((tk = tokenizer.readToken()).getAttribute() == Tokenizer.OPERATOR
					&& !tk.seq.equals(",")) {
				if (opManager.opPrio(tk.seq,"xfx") == prior) {
					Term ta = exprA_inStruct(opManager.opNext(prior));
					if(ta != null){
						return(new Struct(tk.seq,tb,ta));
					}
				}
				if (opManager.opPrio(tk.seq,"xfy") == prior) {
					Term ta = exprA_inStruct(prior);
					if(ta != null){
						return(new Struct(tk.seq,tb,ta));
					}
				}
				if (opManager.opPrio(tk.seq,"xf") == prior){
					return(new Struct(tk.seq,tb));
				}
			}
			tokenizer.unreadToken(tk);
		}
		return(tb);
	}
	
	// helpers
	
	/**
	 * finds a var already present in a list
	 */
	/*
	 private Var findIn(String name,alice.util.LinkedList l) {
	 while(!l.isEmptyList()) {
	 Var v = (Var)l.head;
	 l = l.tail;
	 if(v.getName().equals(name)){
	 return(v);
	 }
	 }
	 return(null);
	 }*/
}