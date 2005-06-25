/* Generated By:JJTree: Do not edit this line. ASTMember.java */

package org.cs3.pl.parser.internal.term;

public class ASTMember extends SimpleNode {
	SimpleNode head=null;
	SimpleNode modulePrefix=null;
	
	SimpleNode body=null;
	public ASTMember(int id) {
		super(id);
	}

	public ASTMember(PrologTermParser p, int id) {
		super(p, id);
	}

	/** Accept the visitor. * */
	public Object jjtAccept(PrologTermParserVisitor visitor, Object data) {
		return visitor.visit(this, data);
	}

	public SimpleNode getHeadLiteral() {
		return head;

	}
	
	
	
		
	public SimpleNode getModulePrefix(){
		return modulePrefix;
	}

	public String getModuleName(){
		SimpleNode node = getModulePrefix();
		if(node==null){
			return ((ASTCompilationUnit)jjtGetParent()).getModuleName();
		}
		return node.getLabel();
	}
	protected void synthesizeImage(StringBuffer sb) {
		((SimpleNode) children[0]).synthesizeImage(sb);
		
	}

	public SimpleNode createShallowCopy() {
		ASTMember copy = new ASTMember(parser,id);
		copy.copy=true;		
		return copy;
	}

	public boolean isDirective() {
		return head==null;
	}
	
	public boolean isFact(){
		return body==null;
	}
	
	public boolean isRule() {
		return !(isDirective()||isFact());

	}
	
	public SimpleNode getPrincipal() {
		return (SimpleNode) children[0];
	}

	public SimpleNode getBody() {
		return body;
	}
	
}
