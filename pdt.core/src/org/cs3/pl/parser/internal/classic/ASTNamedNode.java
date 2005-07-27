/*
 * Created on 31.03.2004
 *
 */
package org.cs3.pl.parser.internal.classic;

/**
 * @author windeln
 *
 */
public class ASTNamedNode extends SimpleNode {
	
	protected String name;
	
	
	public ASTNamedNode(int id) {
		super(id);
	}
	
	/**
	 * @param p
	 * @param i
	 */
	public ASTNamedNode(PrologParser p, int i) {
		super(p, i);
	}
	
	
	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
