/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.transform.internal;

import java.util.HashMap;

import org.cs3.prolog.common.Option;
import org.cs3.prolog.common.OptionProvider;
import org.cs3.prolog.load.PrologLibraryManager;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;

public abstract class PrologRefactoringInfo implements OptionProvider{

	protected HashMap<String,String> parameters = new HashMap<String, String>();
	
	
	@Override
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

	@Override
	public void reconfigure() {
		;
		
	}

	@Override
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


