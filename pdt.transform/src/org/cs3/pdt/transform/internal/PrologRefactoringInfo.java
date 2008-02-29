package org.cs3.pdt.transform.internal;

import java.util.HashMap;

import org.cs3.pl.common.Option;
import org.cs3.pl.common.OptionProvider;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;

public abstract class PrologRefactoringInfo implements OptionProvider{

	protected HashMap<String,String> parameters = new HashMap<String, String>();
	
	
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
	public abstract String getRefactoringId(); 
	abstract public String getName();
	public abstract PrologInterface getPrologInterace();	
	abstract public void configure(PrologLibraryManager libman,PrologSession s) throws PrologInterfaceException;
	abstract public String getSelectionTerm(); 
	abstract public String getParameterTerm();

	
}
