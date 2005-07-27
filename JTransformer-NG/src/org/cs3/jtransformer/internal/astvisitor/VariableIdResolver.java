package org.cs3.jtransformer.internal.astvisitor;

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
		String id = super.getID(node);
		return addPrefix(id);  
	}

	/**
	 * @param id
	 * @return
	 */
	private String addPrefix(String id) {
		if(id.equals("'null'") ||id.equals("null"))
			return id;
		return VARPREFIX + id;
	}

	public String getID(IBinding iface) {
		if (iface == null)
			throw new IllegalArgumentException("Binding was not resolved");
		if (localBindings.containsKey(iface))
			return addPrefix(localBindings.get(iface).toString());
		String id = addPrefix(provider.getID());
		localBindings.put(iface.getKey(),id);
		return id;
	}

	
}
