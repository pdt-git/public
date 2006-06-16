package org.cs3.jtransformer.internal.actions;

import java.util.Iterator;

import org.cs3.jtransformer.internal.astvisitor.FQNTranslator;
import org.cs3.jtransformer.internal.astvisitor.IIDResolver;
import org.cs3.jtransformer.internal.astvisitor.VariableTypeResolver;
import org.cs3.pl.common.Debug;
import org.eclipse.jdt.core.dom.IPackageBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;

public class AddingVariableTypeResolver extends VariableTypeResolver {

	public AddingVariableTypeResolver(FQNTranslator fqnresolve, IIDResolver idresolver) {
		super(fqnresolve, idresolver);
	}
	
	public String getTypeBindungs(){
		StringBuffer buf = new StringBuffer();
		if (types.size() > 0) {
			buf.append("%%% referenced types %%%\n");
			for (Iterator iterator = types.keySet().iterator(); iterator.hasNext();) {
				ITypeBinding typeBinding = (ITypeBinding) iterator.next();
				while(typeBinding.isArray())
					typeBinding = typeBinding.getElementType();
				String var = (String)types.get(typeBinding);
				
				if(typeBinding.isPrimitive())
					continue;
				IPackageBinding packageBinding = typeBinding.getPackage();
				if (packageBinding == null) //
					System.out.println("DEBUG");
				if (!typeBinding.isNested())
					buf.append("add(packageT(Pckg" + var +", '" +
							typeBinding.getPackage().getName()+"')),\n");
				else 
					Debug.warning("VariableTypeResolver: The nested class '" + typeBinding.getQualifiedName() + "' will not be fully qualified.");
				buf.append("add(classDefT("+var+", Pckg" + var +", '" +typeBinding.getName()+"',_)),\n");
			}
			buf.append("\n%%% selection %%%\n");
		}
		return buf.toString();
	}

}
