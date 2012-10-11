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

package org.cs3.pdt.transform.handlers;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.IParameterValues;

public class ExecuteRefactoringParameterValues implements IParameterValues {

	@Override
	public Map<String, String> getParameterValues() {
		HashMap<String,String> m = new HashMap<String, String>();
		m.put("Hash mich.", "fruehling");
		m.put("Hashisch.", "sommer");
		return m;
	}

}


