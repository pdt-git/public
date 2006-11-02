package alice.tuprolog;

import java.util.WeakHashMap;

/**
 * @author alex benini
 *
 * Dictionary of term's names to increase efficency in unification
 */
public class SymbolMap {
	
	/* symbols map */
	private WeakHashMap symbolsMap = new WeakHashMap();
	
	public String getValidName(String keyName){
		String validName = (String)(symbolsMap.get(keyName));
		if (validName==null) {
			validName=keyName;
			symbolsMap.put(keyName,validName);
		}
		return validName;
	}
	
}