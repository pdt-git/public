/*
 * Created on 04.05.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.jlmp.internal.bytecode;

import java.util.Map;
import java.util.TreeMap;

import org.cs3.jlmp.internal.astvisitor.FQNTranslator;
import org.cs3.jlmp.internal.astvisitor.IIDGenerator;

/**
 * @author schulzs1
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ITypeFQNManager implements FQNTranslator {
	/**
	 * @see org.cs3.jlmp.internal.astvisitor.FQNTranslator#getFQNMapping()
	 */
	Map fqnToIDTable = new TreeMap();
	IIDGenerator provider;
		
	public ITypeFQNManager(IIDGenerator provider){
		this.provider = provider;
	}
		
	/** (non-Javadoc)
	 * @see org.cs3.jlmp.internal.astvisitor.FQNTranslator#getFQNMapping()
	 */
	public Map getFQNMapping() {
	
			return fqnToIDTable;
		
	}

	/** (non-Javadoc)
	 * @see org.cs3.jlmp.internal.astvisitor.FQNTranslator#transformFQN(java.lang.String)
	 */
	public String transformFQN(String string) {
			synchronized (fqnToIDTable){
			if (!fqnToIDTable.containsKey(string)){
				fqnToIDTable.put(string, provider.getID());
			}
			return (String) fqnToIDTable.get(string);
		}
	}
}
