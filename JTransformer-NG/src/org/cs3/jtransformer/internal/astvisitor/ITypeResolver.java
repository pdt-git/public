package org.cs3.jtransformer.internal.astvisitor;

import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeLiteral;
/**
 * This interface provides the framework for implementations of the generation
 * of Type-Term resolvers, that map literals and types (from the eAST) to 
 * type(kind, name, dim)-Expressions. None of the Methods allows null, and
 * all but isTypeTerm throw IllegalArgumentExceptions. isTypeTerm returns false
 * on null. Format for a type term is "type(X, Y, Z)", where X is either basic
 * or class, Y is either the primitive name, or the fully qualified name of the
 * class, and Z is the Array Dimension (0 for simple types)
 * 
 * @author schulzs1
 */
public interface ITypeResolver {
	/**
	 * returns a type term for an ITypeBinding. 
	 * @param type the ITypeBinding to be resolved
	 * @return a type(...) term
	 */
	
	public String getTypeTerm(ITypeBinding type);
	
	/**
	 * Get a Type-Term that represents the Type-Object passed
	 * @param type a type Object from the eAST
	 * @return a String that is a valid type-Term
	 */
	public String getTypeTerm(Type type);
	
	/**
	 * Get a Type-Term that represents the Type-Object passed, adding a number 
	 * of extra array dimensions (DONT BLAME ME! BLAME ECLIPSE!)
	 * 
	 * @param type a type Object from the eAST
	 * @param i Extra dimensions to add to the term
	 * @return a String that is a valid type-Term
	 */
	public String getTypeTerm(Type nodetype, int i);
	
	/**
	 * returns a type representation of the StringLiteral
	 * @param m a StringLiteral
	 * @return a String that is a valid type-Term
	 */
	public String getTypeTerm(StringLiteral m);
	
	/**
	 * returns a type representation of the CharacterLiteral
	 * @param c the CharacterLiteral
	 * @return a String that is a valid type-Term
	 */
	public String getTypeTerm(CharacterLiteral c);
	
	/**
	 * returns a String that represents the null-Literal
	 * @param n a NullLiteral
	 * @return "type(basic, null, 0)"
	 */
	public String getTypeTerm(NullLiteral n);
	
	/**
	 * returns a String representation of the BooleanLiteral passed 
	 * @param b a BooleanLiteral
	 * @return a String that is a valid type-Term
	 */
	public String getTypeTerm(BooleanLiteral b);
	
	/**
	 * returns a String representation of the NumberLiteral passed. This 
	 * has the form type(basic, X, 0), where X is one of int, long, float and 
	 * double
	 * @param n a NumberLiteral
	 * @return the proper type-term
	 */
	public String getTypeTerm(NumberLiteral n);
	
	/**
	 * returns a representation of the referenced TypeLiteral, which can either
	 * be a basic type, or class name.
	 * @param n a Type-Literal 
	 * @return an type-Term that represents the TypeLiteral
	 */
	public String getTypeTerm(TypeLiteral n);
	
	/**
	 * Checks whether a given String is a valid type-Term (only syntax-checking,
	 * no check is made whether referenced types can be resolved in the 
	 * classpath)
	 * @param str a String to be checked
	 * @return whether the String is a valid type(...) term.
	 * @deprecated i see no point in this method, it is never called. will be removed.
	 */
	public boolean isTypeTerm(String str);	
}
