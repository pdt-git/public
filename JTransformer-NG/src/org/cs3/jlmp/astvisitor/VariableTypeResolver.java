package org.cs3.jlmp.astvisitor;

import java.util.Hashtable;
import java.util.Iterator;

import org.cs3.pl.common.Debug;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.Type;

/**
 * Provides type terms fit to be used in the "Selection to Query"
 * feature.
 * @author xproot
 * @inheritDoc
 */
public class VariableTypeResolver extends TypeResolver {
	private Hashtable types = new Hashtable();
	private int varnum = 0;
	
	private Hashtable vars = new Hashtable();
	
	protected String getClassTypeTerm(String fqn, int dim) {
		String cid;
		if ((cid = (String)vars.get(fqn)) == null) {
			cid = "Type" + varnum++;
			vars.put(fqn,cid);
		}
			
		return "type(class, "+cid + ", "+dim+ ")";
	}

	/**
	 * returns the referenced types, that were "seen" by this
	 * VariableTypeResolver. The types are returned as a String,
	 * consisting of multiple lines, one fact or comment
	 * per line.
	 * 
	 * @return A String representing the type Bindings that were
	 * seen by the resolver 
	 */
	
	public String getTypeBindungs(){
		StringBuffer buf = new StringBuffer();
		if (types.size() > 0) {
			buf.append("%%% referenced types %%%\n");
			for (Iterator iterator = types.keySet().iterator(); iterator.hasNext();) {
				ITypeBinding typeBinding = (ITypeBinding) iterator.next();
				String var = (String)types.get(typeBinding);
				if (!typeBinding.isNested())
					buf.append("packageT(Pckg" + var +", '" +typeBinding.getPackage().getName()+"'),\n");
				else 
					Debug.warning("VariableTypeResolver: The nested class '" + typeBinding.getQualifiedName() + "' will not be fully qualified.");
				buf.append("classDefT("+var+", Pckg" + var +", '" +typeBinding.getName()+"',_),\n");
			}
			buf.append("\n%%% selection %%%\n");
		}
		return buf.toString();
	}

	public String getTypeTerm(ITypeBinding bind) {
		String str =  super.getTypeTerm(bind);
		storeType(bind);
		return str;
	}
	
	public String getTypeTerm(StringLiteral m) {
		String str =  super.getTypeTerm(m);
		storeType(m.resolveTypeBinding());
		return str;
	}
	
	public String getTypeTerm(Type type, int extradim) {
		String str =  super.getTypeTerm(type,extradim);
		if (!type.isPrimitiveType() && 
			!(type.isArrayType() && 
			((ArrayType)type).getElementType().isPrimitiveType()))
			storeType(type.resolveBinding());
		return str;
	}
	
	
	private void storeType(ITypeBinding bind) {
		String fqn = bind.getQualifiedName();
		if (types.get(fqn) == null) {
			String var = (String)vars.get(fqn);
			types.put(bind,var);
		}
	}

}
