/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.common;


public interface OptionProvider {

	public Option[] getOptions();
	
	public void reconfigure();

	
	/* maybe the following two should be in some other interface?
	 * pif factories don't support them (now), still they provide options
	 * 
	 * preference pages cannot quiet use them... afaics
	 */ 
	public String getPreferenceValue(String id, String string);
	public void setPreferenceValue(String id, String value);
	

}


