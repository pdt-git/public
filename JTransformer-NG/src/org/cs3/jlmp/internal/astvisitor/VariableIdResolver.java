package org.cs3.jlmp.internal.astvisitor;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.IBinding;

/**
 * Provides IDs that are Variables in the Prolog System, 
 * for the "Selection to Query" feature.
 * 
 * @author windeln
 * @inheritDoc
 */
public class VariableIdResolver extends IDResolver {

	private static final String VARPREFIX = "V";

	public VariableIdResolver(){
		super (new IdentityFQNTranslator(), new SimpleIDGenerator(0));
	}
	
//
//	public String getEmptyList(){
//		return "[]";
//	}
//
//	public void reset() {
//		provider = new SimpleIDGenerator(0);
//		localBindings = new HashMap();
//		knownIDs = new HashMap();
//	}

	public String getID(ASTNode node){
		return VARPREFIX +  super.getID(node);
	}

	public String getID(IBinding iface) {
		if (iface == null)
			throw new IllegalArgumentException("Binding was not resolved");
		if (localBindings.containsKey(iface))
			return VARPREFIX +  localBindings.get(iface).toString();
		String id = VARPREFIX +  provider.getID();
		
		localBindings.put(iface.getKey(),id);
		return id;
	}

	
}
