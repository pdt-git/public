package org.cs3.pdt.runtime.internal;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pl.prolog.PrologInterface;
/**
 * the implemetnations assumes that performance is not an issue - after all, how
 * many pifs will be active at a time?  How often will this interface be accessed?
 * Both answers are assumed to lie safely below 100
 * If this assumptions prove wrong, we need to invest more work in this implementation.
 */
public class DefaultPrologInterfaceRegistry implements PrologInterfaceRegistry {

	private static class _Record{
		PrologInterface pif;
		String use;
		Object client;
		public _Record(PrologInterface pif, Object client, String use) {
			this.pif=pif;
			this.client=client;
			this.use=use;
		}
		public boolean equals(Object obj) {
			if(obj instanceof _Record){
				_Record r = (_Record) obj;
				return pif==r.pif&&client==r.client&&
				(use==null&&r.use==null||use.equals(r.use));
			}
			return super.equals(obj);
		}
		public int hashCode() {
			int h=0;
			if(pif!=null){
				h+=pif.hashCode();
			}
			if(client!=null){
				h+=client.hashCode();
			}
			if(use!=null){
				h+=use.hashCode();
			}
			return h;
		}
	}
	
	Map records = new HashMap();
	
	public void register(PrologInterface pif, Object client, String use) {
		_Record r = new _Record(pif,client,use);
		Set s = (Set) records.get(pif);
		if(s==null){
			s=new HashSet();
			records.put(pif,s);
		}
		s.add(r);
	}

	public void unregister(PrologInterface pif, Object client, String use) {
		_Record r = new _Record(pif,client,use);
		Set s = (Set) records.get(pif);
		if(s==null){
			s=new HashSet();
			records.put(pif,s);
		}
		s.remove(r);

	}

	public PrologInterface[] getPrologInterfaces() {
		return (PrologInterface[]) records.keySet().toArray(new PrologInterface[records.size()]);
	}

	
	public Map getClients(PrologInterface pif) {
		Map m = new HashMap();
		Set s = (Set) records.get(pif);
		if (s==null){
			return null;
		}
		for (Iterator it = s.iterator(); it.hasNext();) {
			_Record r = (_Record) it.next();
			Set t = (Set) m.get(r.use);
		}
		return null;
	}

	
}
