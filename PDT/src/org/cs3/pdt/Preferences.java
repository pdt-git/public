package org.cs3.pdt;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;
import java.util.Set;
import java.util.Map;

public class Preferences implements IPreferences {

	private Vector listeners = new Vector();
	private HashMap data = new HashMap();
	public void addPreferencesListener(PreferenceListener l){
		synchronized(listeners){
			if(!listeners.contains(l)){
				listeners.add(l);
			}
		}
	}
	public void removePreferencesListener(PreferenceListener l){
		synchronized(listeners){
			if(listeners.contains(l)){
				listeners.remove(l);
			}		
		}
	}
	
	public String get(String key){
		synchronized(data){
			return (String) data.get(key);
		}
	}
	
	public String get(String key, String dfltVal){
		String object = get(key);		
		return object==null? dfltVal:object;
	}
	
	public void set(String key, String value){
		synchronized(data){
			data.put(key,value);
		}
		Set keys=new HashSet();
		keys.add(key);
		PreferencesEvent e = new PreferencesEvent(this,keys);
		firePreferencesChanged(e);
	}
	
	public void set(Map keyToValue){
		synchronized(data){
			data.putAll(keyToValue);
		}
		PreferencesEvent e = new PreferencesEvent(this,keyToValue.keySet());
		firePreferencesChanged(e);
	}
	
	private void firePreferencesChanged(PreferencesEvent e) {
		Vector cloned = null;
		synchronized(listeners){
			cloned = (Vector)listeners.clone();
		}
		for (Iterator it = cloned.iterator(); it.hasNext();) {
			PreferenceListener l = (PreferenceListener) it.next();
			l.preferencesChanged(e);
		}
	}
	
	
	
}
