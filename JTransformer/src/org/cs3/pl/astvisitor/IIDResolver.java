/*
 * Created on 18.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.astvisitor;

import java.util.List;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.IBinding;

/**
 * Provides a mapping between ASTNodes and the Bindings presented by them
 * and (mostly) unique identifierers. The Identifiers are presented as
 * Strings and can take a variety of forms.
 * 
 * @author degenerl
 */
public interface IIDResolver extends IIDGenerator {
	
	/**
	 * returns the unique identifier for the ASTNode. This is either an 
	 * fqn-Term, a numeric id, an lId-Term, or yet another presentation.
	 * 
	 * <u><b>DO NOT RELY ON ANY ONE OF THEM</b></u>
	 * 
	 * @param node 
	 * @return a string representing the node.
	 */
	
	public String getID(ASTNode node);
	
	/**
	 * returns the unique identifier for the object bound. This can either
	 * be a fqn-Term (for interface elements), or a numeric ID (for class 
	 * internals), or something else entirely. Two nodes will get 
	 * different id's <u>unless</u> there has been an explicity call to 
	 * setEquivalence before resolution of the second item.
	 * 
	 * @param iface
	 * @return
	 */
	
	public String getID(IBinding iface);
	
	/**
	 * Returns a Prolog-List with the IDs for the ASTNodes and IBindings in the
	 * List passed as the arguments
	 * @param nodes A list of ASTNodes and IBindings
	 * @return A prolog-Style list, as a String. Has the form [id1, id2, ...]
	 */
	
	public String getIDs(List nodes);
	
	/**
	 * Makes sure a pair of nodes is treated as equal in all respects. This
	 * means that any reference to these nodes is treated as a reference to the
	 * original node. This is an exception to the normal 1 node -> 1 id rule!
	 * 
	 * This means, that for each call for getID(n2) the ID returned by getID(n1)
	 * is returned!
	 * 
	 * @param n1 Node to be used as the key
	 * @param n2 Node to be treated as equal to the key
	 */
	public void setEquivalence(ASTNode n1, ASTNode n2);

	/**
	 * Helper method, returns the String representation of the java.lang.Object
	 * class.
	 * 
	 * @return a String, appropriate for the resolver, representing java.lang.Object
	 */
	public String getJavaLangObjectID();
	
	/**
	 * Helper method, returns the String representation of the java.lang.Class
	 * class. This method is necessary to accomodate our many and varied 
	 * encodings for fully qualified names.
	 * 
	 * @return a String, appropriate for the resolver, representing java.lang.Class
	 */
	
	public String getJavaLangClassID();
}
