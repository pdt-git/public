/*
 * Created on 03.03.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.jtransformer.internal.bytecode;

import java.util.HashMap;
import java.util.Map;

import org.cs3.jtransformer.internal.astvisitor.FQNTranslator;
import org.cs3.jtransformer.internal.astvisitor.IIDGenerator;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;

/**
 * @author schulzs1
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class IDManagerIType {

	
	private HashMap known = new HashMap();
	
	

	private IType envClass;

	private boolean LOCALIDS=true;//"Sowieso!" findet Lukas.
	
	private FQNTranslator fqn;
	private IIDGenerator ids;
	
	public IDManagerIType(FQNTranslator fqn, IIDGenerator ids){		
		this.fqn = fqn;
		this.ids = ids;
	}
	
//	public IDManagerIType(){
//		manager = PDTPlugin.getDefault().getPrologClient();
//		fqn = new IdentityFQNTranslator();
//		this.ids = new SimpleIDGenerator();
//	}

	public String getID(IType type) throws JavaModelException {
		return fqn.transformFQN("fqn('" + getFullyQualifiedName(type)+"')");
	}

	public String getID(String type) throws JavaModelException {
		return fqn.transformFQN("fqn('" + getTypeName(type)+"')");
	}

	
	/**
	 * @return
	 */
	public String getID(IField field) throws JavaModelException {
		return fqn.transformFQN("fqn('" + getFullyQualifiedName(field.getDeclaringType())+ "', '" + field.getElementName() + "')");
	}

	public String getID(IMethod method) throws JavaModelException {
		String name = method.isConstructor() ? "<init>" : method.getElementName();
		return fqn.transformFQN("fqn('" + getFullyQualifiedName(method.getDeclaringType()) + "', '" +
		name + "',"+ arglist(method.getParameterTypes())+")");
	} 

//	public String getID(Member o){
//		if (o instanceof Method){
//			Method m = (Method) o;
//			return "fqn('" + m.getDeclaringClass().getName() + "', '" + m.getName() + "', " + arglist(m.getParameterTypes()) + ")";
//		} else if (o instanceof Constructor){
//			Constructor c = (Constructor) o;
//			return "fqn('" + c.getDeclaringClass().getName() + "', '<init>', " + arglist(c.getParameterTypes()) + ")";
//		}
//		
//		Field f = (Field) o;
//		return "fqn('" + f.getDeclaringClass().getName() + "', '" + f.getName() +"')";
//	}


	/**
	 * @param type
	 * @return
	 */
	private String getFullyQualifiedName(IType type) {
		return type.getFullyQualifiedName('$'); //TW: FQN$	
	}

	/**
	 * @param p
	 * @return
	 */
	public boolean knowsPackage(PrologSession s,Package p) {
		if (known.containsKey(p))
			return true;
		else {
			/* Prolog lookup */
			boolean rv = lookupPackage(s,p);
			known.put(p, new Boolean(rv));
			return rv;
		}
	}

//	/**
//	 * @param class1
//	 * @return
//	 */
//	public boolean knowsClass(Class	class1) {
//		if (known.containsKey(class1))
//			return ((Boolean)known.get(class1)).booleanValue();
//		else {  Prolog Lookup 
//			boolean rv = lookupClass(class1);
//			known.put(class1, new Boolean(rv));
//			return rv;
//		}	
//	}
	
	/**
	 * @return
	 */
	public String newID() {
		return ids.getID();
	}
	
	private String getSimpleName(String classname){
		try {
			classname = classname.substring(classname.lastIndexOf("."));
		} catch (IndexOutOfBoundsException e) {

		}
		return classname;
	}

	

   /**
 	* @param classes
 	* @return
 	*/
	private String arglist(String[] classes) throws JavaModelException {
		StringBuffer buf = new StringBuffer("[");
		boolean first = true;
		
		
		for (int i = 0; i < classes.length; i++){
			String type = classes[i];
			int level = 0;
			
			
			if (!first)
				buf.append(", ");
			else
				first = false;
			
			buf.append("'");
			int dim  = getArrayDim(type);
			buf.append(getTypeName(type.substring(dim)));
			for (; dim > 0; dim--)
				buf.append("[]");
			buf.append("'");
		}
		
		buf.append("]");
		
		return buf.toString();
	}
	
	/**
	 * 
	 * See Java VM Spec: 4.3.2 Field Descriptors
 * @param string
 * @return
 */
	public String getTypeName(String s) throws JavaModelException {
	switch(s.charAt(0)){
		case 'B':
			return "byte";
		case 'C':
			return "char";
		case 'D':
			return "double";
		case 'F':
			return "float";
		case 'I':
			return "int";
		case 'J':
			return "long";
		case 'L':
			return s.substring(1,s.length()-1);//.replace('$','.');
		case 'S':
			return "short";
		case 'V':
			return "void";
		case 'Z':
			return "boolean";
		case 'Q':
			Debug.warning("The class " + envClass.getFullyQualifiedName() + " is available in SC.");
			String[] aFqn = null;
				String simpleClassName;
				// TODO: warum ist mal ein ; da und manchmal nicht?
				if(s.charAt(s.length()-1) == ';')
					simpleClassName = s.substring(1,s.length()-1);
				else
					simpleClassName = s.substring(1);
 				String fqn = resolveClassNameFromSimpleName(simpleClassName);
			return fqn;
		default:
			throw new RuntimeException("can not find type for: " + s);
	}
}

/**
	 * @param simpleClassName
	 * @return
	 * @throws JavaModelException
	 */
	private String resolveClassNameFromSimpleName(String simpleClassName) throws JavaModelException {
		String[] aFqn;
		aFqn = envClass.resolveType(simpleClassName)[0];
		String fqn;
		if(aFqn.length == 1)
			fqn = aFqn[0].replace('.','$');
		else if (aFqn.length == 2)		
			fqn = aFqn[0] + "." + aFqn[1].replace('.','$');
		else
			throw new RuntimeException("Unexpected arity of fqn array: " + aFqn.length + ", " + aFqn[0]);
		return fqn;
	}

public static boolean isPrimitive(String s) {
	switch(s.charAt(0)){
		case '[':
			throw new RuntimeException("no arrayType excpected: " + s);
		case 'L':
			return false;
		case 'Q':
			return false;
		default:
			return true;
	}
}

	/**
 * @param type
 * @return
 */
public static int getArrayDim(String type) {
	int dim = 0;
	while(type.charAt(dim) == '[')
		dim++;
	return dim;
}

	/**
	 * @param p
	 * @return
	 */
	private boolean lookupPackage(PrologSession session,Package p) {
		Map h = session.queryOnce("packageT(X, '" + p.getName() +"')");
		if (h != null && h.containsKey("X"))
			return true;
		
		return false;
	}
	
	/**
	 * @param targetClass
	 */
	public void setEnvironment(IType type) {
		this.envClass = type;
		
	}

	/**
	 * @param string
	 * @return
	 */
	public String resolveType(String string) throws JavaModelException {
		if (envClass.resolveType(string) == null) // byte code class
			return "fqn('"+string+"')";
		return "fqn('"+resolveClassNameFromSimpleName(string)+"')";
//		String[][] str = envClass.resolveType(string);
//		StringBuffer buf = new StringBuffer();
//		for (int i = 0; i < str[0].length; i++) {
//			if(i > 0)
//			buf.append('.');
//			buf.append(str[0][i]);
//			
//		}
//		return "fqn('"+buf.toString()+"')";
	}
}
