/*
 * Created on 20.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.jlmp.internal.astvisitor;

import java.util.Vector;

import org.eclipse.jdt.core.dom.*;

/**
 * Provides resultion from Type objects and Bindings to type(...) terms.
 * 
 * @author schulzs1
 * @inheritDoc
 */
public class TypeResolver implements ITypeResolver {
	
	private FQNTranslator fqnresolve;
    private IIDResolver idResolver;
	
	/**
	 * constructs a new TypeResolver. The FQNManager is used to
	 * translate fqn(...) terms.
	 * @param fqnresolve an FQNManager translating fqn-terms
	 */
	
	public TypeResolver(FQNTranslator fqnresolve, IIDResolver idresolver){
		this.fqnresolve = fqnresolve;
		this.idResolver=idresolver;
	}
	
	/**
	 * constructs a new TypeResolver. This constructor does not
	 * translate fqn(...) terms at all.
	 * @deprecated
	 */

	public TypeResolver(){
		fqnresolve = new IdentityFQNTranslator();
	}
	
	
	public String getTypeTerm(Type nodetype) {
		return getTypeTerm(nodetype, 0);
	}
		
	public String getTypeTerm(Type type, int extradim) {
		if (type == null)
			throw new IllegalArgumentException("null passed to TypeResolver");
		
		int dim = extradim;
		if(type.isArrayType()) {
			ArrayType at = (ArrayType) type;
			dim += at.getDimensions();
			type = at.getElementType();
		}
		
		if (type.isPrimitiveType()){
			PrimitiveType pt = (PrimitiveType) type;
			return "type(basic, " + pt.getPrimitiveTypeCode() + ", "+ dim +")";
		} else { /* SimpleType */
			SimpleType st = (SimpleType) type;
			Name name = st.getName();
			ITypeBinding bind = (ITypeBinding) name.resolveBinding();
			if (name.isQualifiedName()){
				return getClassTypeTerm( getFullyQualifiedName(bind), dim ); //TODO: FQN$
			} else {
				if (bind == null)
					throw new IllegalArgumentException("Type name can not be qualified");
				
				return getClassTypeTerm( getFullyQualifiedName(bind), dim);
			}
		}
	}
	
	/**
	 * @param bind
	 * @return
	 */
	private String getFullyQualifiedName(ITypeBinding bind) {
//		
//	    String buf = bind.getName();
//		while(bind.getDeclaringClass() != null) {
//			bind = bind.getDeclaringClass();
//			buf = bind.getName() + "$" + buf;
//		}
//		return bind.getPackage().isUnnamed() ? buf : bind.getPackage().getName() + "." + buf;
	    
// ld: bla bla bla... why not this way:
        ///ld: XXX: this is a quick workaround. the whole package should be 
        //cleaned up.        
	    return IDResolver.normalizeFullQualifiedName(bind.getKey());
	    
	}

	/**
	 * @param string
	 * @param i
	 * @return
	 */
	protected String getClassTypeTerm(String fqn, int dim) {
		String fqnTerm = "fqn('" + fqn + "')";
        String rv = "type(class, " + fqnresolve.transformFQN(fqnTerm) + ", "+dim+ ")";
		return rv;
	}
	  /**
     * @param type
     * @param dim
     * @return
     */
    private String getClassTypeTerm(ITypeBinding type, int dim) {
        String rv = "type(class, " + idResolver.getID(type) + ", "+dim+ ")";
		return rv;
    }
	/**
	 * @see org.cs3.jlmp.internal.astvisitor.ITypeResolver#getTypeTerm(org.eclipse.jdt.core.dom.StringLiteral)
	 */
	
	public String getTypeTerm(StringLiteral m) {
		if (m == null)
			throw new IllegalArgumentException("null passed to TypeResolver");
		return getClassTypeTerm("java.lang.String", 0);
	}
	
	/**
	 * @see org.cs3.jlmp.internal.astvisitor.ITypeResolver#getTypeTerm(org.eclipse.jdt.core.dom.CharacterLiteral)
	 */
	
	public String getTypeTerm(CharacterLiteral c) {
		if (c == null)
			throw new IllegalArgumentException("null passed to TypeResolver");
		return "type(basic, char, 0)";
	}
	
	/**
	 * @see ITypeResolver#getTypeTerm(NullLiteral)
	 */
	
	public String getTypeTerm(NullLiteral n) {
		if (n == null)
			throw new IllegalArgumentException("null passed to TypeResolver");
		return "type(basic, null, 0)";
	}
	
	/**
	 * @see ITypeResolver#getTypeTerm(BooleanLiteral)
	 */
	
	public String getTypeTerm(BooleanLiteral b) {
		if (b == null)
			throw new IllegalArgumentException("null passed to TypeResolver");
		return "type(basic, boolean, 0)";
	}
	/**
	 * @see org.cs3.jlmp.internal.astvisitor.ITypeResolver#getTypeTerm(org.eclipse.jdt.core.dom.NumberLiteral)
	 */
	public String getTypeTerm(NumberLiteral n) {
		if (n == null)
			throw new IllegalArgumentException("null passed to getTypeTerm");
		
		String choice;
		String token = n.getToken();
		
		if (token.endsWith("l") || token.endsWith("L"))
			return "type(basic, long, 0)";
		else if (token.endsWith("d") || token.endsWith("D"))
			return "type(basic, double, 0)";
		else if (token.endsWith("f") || token.endsWith("F"))
			return "type(basic, float, 0)";
		
		try {
			long lvalue = Long.parseLong(token);
			if (lvalue > Integer.MAX_VALUE || lvalue < Integer.MIN_VALUE)
				return "type(basic, long, 0)";
			return "type(basic, int, 0)";
		} catch (NumberFormatException e){
			return "type(basic, double, 0)";
		}
	}
	
	/**
	 * @see org.cs3.jlmp.internal.astvisitor.ITypeResolver#getTypeTerm(org.eclipse.jdt.core.dom.TypeLiteral)
	 */
	public String getTypeTerm(TypeLiteral n) {
		if (n == null)
			throw new IllegalArgumentException("null passed to TypeResolver");
		return getClassTypeTerm("java.lang.Class", 0);
	}
	
	/**
	 * @see org.cs3.jlmp.internal.astvisitor.ITypeResolver#isTypeTerm(java.lang.String)
	 */
	public boolean isTypeTerm(String str) {
		if (str == null || str.length() == 0)
			return false;
		
		char [] chars = str.toCharArray();
		
		if (chars[chars.length - 1] != ')')
			return false;
		
		if (!str.startsWith("type("))
			return false;
		
		StringBuffer type = new StringBuffer();
		StringBuffer name = new StringBuffer();
		StringBuffer num = new StringBuffer();
		
		StringBuffer [] sbs = new StringBuffer[] {type, name, num};
		int index = 0;
		
		for (int i = 5; i < chars.length; i++) {
			char c = chars[i];
			
			if (c == ','){
				i++;
				index++;
				
				if (chars[i] != ' ' || index >= 3)
					return false;
				
				continue;
			}
			
			sbs[index].append(c);
		}
		
		if (index < 2)
			return false;
		
		int number;
		String fqn = name.toString();
		String foo = num.toString();
		
		if (!foo.endsWith(")"))
			return false;
		else foo = foo.substring(0, foo.length() - 1);		
		
		try {
			number = Integer.parseInt(foo);
		} catch (NumberFormatException e){
			return false;
		}
		
		if (type.toString().equals("basic")){		
			if (fqn.equals("int") || fqn.equals("long") || fqn.equals("double") || fqn.equals("float"))
				return number >= 0;
			
			if (fqn.equals("void") || fqn.equals("null"))
				return number == 0;
			
		} else if (type.toString().equals("class")){
			if (number < 0)
				return false;
			
			if (!fqn.startsWith("fqn("))
				return false;
			
			fqn = fqn.substring(4);
			
			char [] split = fqn.toString().toCharArray();
			
			if (split[0] != '\'')
				return false;		
			
			if (!Character.isJavaIdentifierStart(split[1]))
				return false;
			
			for (int i = 2; i < split.length - 2; i++) {
				char c = split[i];
				if (!Character.isJavaIdentifierPart(c) && c != '.')
					return false;
			}
			
			if (split [split.length - 2] != '\'')
				return false;
			
			if (split [split.length - 1] != ')')
				return false;
			
			return notReservedWord(fqn.substring(1, fqn.length() - 1));			
		} 
		return false;
	}
	
	private boolean notReservedWord(String type) {
		Vector v = new Vector();
		v.add("abstract");
		v.add("boolean");
		v.add("break");
		v.add("byte");
		v.add("case");
		v.add("catch");
		v.add("char");
		v.add("class");
		v.add("const");
		v.add("continue");
		v.add("default");
		v.add("do");
		v.add("double");
		v.add("else");
		v.add("extends");
		v.add("final");
		v.add("finally");
		v.add("float");
		v.add("for");
		v.add("goto");
		v.add("if");
		v.add("implements");
		v.add("import");
		v.add("instanceof");
		v.add("int");
		v.add("interface");
		v.add("long");
		v.add("native");
		v.add("new");
		v.add("null");
		v.add("package");
		v.add("private");
		v.add("protected");
		v.add("public");
		v.add("return");
		v.add("short");
		v.add("static");
		v.add("strictfp");
		v.add("super");
		v.add("switch");
		v.add("synchronized");
		v.add("this");
		v.add("throw");
		v.add("throws");
		v.add("transient");
		v.add("try");
		v.add("void");
		v.add("volatile");
		v.add("while");
		return !(v.contains(type));
	}
	
	/**
	 * @see org.cs3.jlmp.internal.astvisitor.ITypeResolver#getTypeTerm(org.eclipse.jdt.core.dom.ITypeBinding)
	 */
	
	public String getTypeTerm(ITypeBinding type) {
		if (type == null)
			throw new IllegalArgumentException("null passed to TypeResolver");
		
		int dim = 0;
		if(type.isArray()) {
			dim += type.getDimensions();
			type = type.getElementType();
		}
		
		if (type.isPrimitive())
			return "type(basic, " + type.getName() + ", "+ dim +")";
		else 
			return getClassTypeTerm( type, dim);

	}

  
}
