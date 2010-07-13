package org.cs3.pl.cterm.internal.parser;

public class ASTNode extends SimpleNode {
	private String functor;
	private String stringRepresentation;

	public ASTNode(int i) {
		super(i);
	}

	public ASTNode(CanonicalTermParser p, int i) {
		super(p, i);
	}

	public String getFunctor() {
		if(functor != null){
			return functor;
		}
		StringBuffer sb = new StringBuffer();
		Token t = jjtGetFirstToken();
		do{
			sb.append(t.image);
			t=t.next;
		} while(canGetAnotherToken(t)&&
			 !t.image.equals("("));
		functor = sb.toString();
		return functor;
	}

	public String getString(){
		if(stringRepresentation != null){
			return stringRepresentation;
		}		
		StringBuffer sb = new StringBuffer();
		for(Token t = jjtGetFirstToken();canGetAnotherToken(t);t=t.next){
			sb.append(t.image);
		}
		stringRepresentation = sb.toString();
		return stringRepresentation;
	}
	
	private boolean canGetAnotherToken(Token t) {
		if (t==null) 
			return false;
		if (t==lastToken.next)
			return false;
		return true;
	}
	
}
