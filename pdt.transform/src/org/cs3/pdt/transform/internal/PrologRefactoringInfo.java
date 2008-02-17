package org.cs3.pdt.transform.internal;

import java.util.HashMap;

import org.cs3.pl.common.Option;
import org.cs3.pl.common.OptionProvider;
import org.cs3.pl.prolog.PrologInterface;

public abstract class PrologRefactoringInfo implements OptionProvider{

	private HashMap<String,String> parameters = new HashMap<String, String>();
	
	public abstract String getHead(); 
	public abstract PrologInterface getPrologInterace();
	
	public String getPreferenceValue(String key, String defaultValue) {
		
		String value = parameters.get(key);
		if(value!=null){
			return value;
		}
		Option[] o = getOptions();
		for (int i = 0; i < o.length; i++) {
			if (o[i].getId().equals(key)) {
				return o[i].getDefault();
			}
		}
		return defaultValue;
	}

	public void reconfigure() {
		;
		
	}

	public void setPreferenceValue(String id, String value) {
			parameters.put(id, value);
		
	}
	abstract public String getName();

	
}
