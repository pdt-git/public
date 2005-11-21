package org.cs3.pl.parser;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

public class NaiveIndex implements Index {

	HashMap referringFiles = new HashMap();
	HashMap referencedKeyes = new HashMap();
	
	public void addReference(String key, String filename) {
		Set s = getReferringFiles(key);
		s.add(filename);
		s=getReferencedKeys(filename);
		s.add(key);
	}

	public void removeReference(String key, String filename) {
		Set s = getReferringFiles(key);
		s.remove(filename);
		s=getReferencedKeys(filename);
		s.remove(key);

	}

	public void removeAllReferences(String filename) {
		Set s = getReferencedKeys(filename);
		HashSet c = new HashSet(s);
		for (Iterator iter = c.iterator(); iter.hasNext();) {
			String key = (String) iter.next();
			removeReference(key,filename);
		}
	}

	public Set getReferringFiles(String key) {
		// TODO Auto-generated method stub
		return null;
	}

	public Set getReferencedKeys(String filename) {
		// TODO Auto-generated method stub
		return null;
	}

}
