/*
 * Created on 04.05.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.bytecode;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Vector;

import org.cs3.pl.astvisitor.FQNTranslator;
import org.cs3.pl.astvisitor.IIDGenerator;
import org.cs3.pl.fileops.PrologWriterAction;

/**
 * @author schulzs1
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ITypeFQNManager implements FQNTranslator {
	/**
	 * @see org.cs3.pl.astvisitor.FQNTranslator#getFQNMapping()
	 */
	Map fqnToIDTable = new TreeMap();
	IIDGenerator provider;
		
	public ITypeFQNManager(IIDGenerator provider){
		this.provider = provider;
	}
		
	/** (non-Javadoc)
	 * @see org.cs3.pl.astvisitor.FQNTranslator#getFQNMapping()
	 */
	public List getFQNMapping() {
	
			Vector list = new Vector();
			
			list.add(PrologWriterAction.newSetInterpret(false));
			
			
			synchronized (fqnToIDTable){
				Collection c = fqnToIDTable.keySet();
				
				for (Iterator iter = c.iterator(); iter.hasNext();) {
					String key = (String) iter.next();
					PrologWriterAction plwa = PrologWriterAction.newWriteFact("local2FQN", 
							new String [] {(String) fqnToIDTable.get(key), key});
					list.add(plwa);
				}
			}
			
			list.add(PrologWriterAction.newSetInterpret(true));
			
			return list;
		
	}

	/** (non-Javadoc)
	 * @see org.cs3.pl.astvisitor.FQNTranslator#transformFQN(java.lang.String)
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
