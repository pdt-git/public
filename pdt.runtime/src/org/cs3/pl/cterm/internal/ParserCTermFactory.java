package org.cs3.pl.cterm.internal;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import org.cs3.pl.common.Debug;
import org.cs3.pl.cterm.CAtom;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CFloat;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CTermFactory;
import org.cs3.pl.cterm.CVariable;
import org.cs3.pl.cterm.internal.parser.ASTAtom;
import org.cs3.pl.cterm.internal.parser.ASTCompound;
import org.cs3.pl.cterm.internal.parser.ASTFloat;
import org.cs3.pl.cterm.internal.parser.ASTInteger;
import org.cs3.pl.cterm.internal.parser.ASTVariable;
import org.cs3.pl.cterm.internal.parser.CanonicalTermParser;
import org.cs3.pl.cterm.internal.parser.Node;
import org.cs3.pl.cterm.internal.parser.ParseException;
import org.cs3.pl.cterm.internal.parser.SimpleNode;

public class ParserCTermFactory implements CTermFactory {

	

	public CTerm createCTerm(Object data) {
		CanonicalTermParser parser=null;
		if(data instanceof InputStream){
			parser = new CanonicalTermParser((InputStream) data); 
		}
		else if (data instanceof Reader){
			parser = new CanonicalTermParser((Reader) data);
		}
		else{
			String input = data.toString();
			Reader reader = new StringReader(input);
			parser = new CanonicalTermParser(reader);
		}
		
		try {
			parser.Term();
		} catch (ParseException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		return create(parser.getASTRoot());
	}

	
	private  class _Atom extends AbstractCTerm implements CAtom{

		public _Atom(SimpleNode node) {
			super(ParserCTermFactory.this, node);
		}
		
	}
	
	private  class _Variable extends AbstractCTerm implements CVariable{

		public _Variable(SimpleNode node) {
			super(ParserCTermFactory.this, node);
		}

		public String getVariableId() {
			return getFunctorValue();
		}
		
	}
	
	private  class _Float extends AbstractCTerm implements CFloat{

		public _Float(SimpleNode node) {
			super(ParserCTermFactory.this, node);
		
		}

		public double getDoubleValue() {			
			return Double.parseDouble(getFunctorValue());
		}

		
		
	}
	private  class _Integer extends AbstractCTerm implements CInteger{

		public _Integer(SimpleNode node) {
			super(ParserCTermFactory.this, node);
		
		}

		public int getIntValue() {		
			return Integer.parseInt(getFunctorValue());
		}
		
	}
	
	private  class _Compound extends AbstractCTerm implements CCompound{

		private CTerm[] args;

		public _Compound( SimpleNode node) {
			super(ParserCTermFactory.this, node);
			args = new CTerm[node.jjtGetNumChildren()-1]; 
		}

		public CTerm getArgument(int i) {
			if(args[i]==null){
				args[i]=create(node.jjtGetChild(i+1));
			}
			return args[i];
		}

		protected String doGetFunctorImage() {
			return ((ASTAtom)node.jjtGetChild(0)).getImage();
		}
	}
	private  CTerm create(Node root) {
		if(root instanceof ASTAtom){
			return new _Atom((SimpleNode) root);
		} 
		if(root instanceof ASTVariable){
			return new _Variable((SimpleNode) root);
		} 
		if(root instanceof ASTCompound){
			return new _Compound((SimpleNode) root);
		} 
		if(root instanceof ASTInteger){
			return new _Integer((SimpleNode) root);
		}
		
		if(root instanceof ASTFloat){
			return new _Float((SimpleNode) root);
		}
		throw new IllegalArgumentException("bad node type: "+root.getClass().getName());
	}

	

	
	}
