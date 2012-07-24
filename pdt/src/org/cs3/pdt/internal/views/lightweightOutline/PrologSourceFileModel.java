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

package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.Map;
import org.cs3.pdt.internal.structureElements.OutlineModuleElement;


public class PrologSourceFileModel {

	private Map<String, OutlineModuleElement> modules;
	
	public PrologSourceFileModel(Map<String, OutlineModuleElement> modules) {
		update(modules);
	}
	
	public void update(Map<String,OutlineModuleElement> modules){
		this.modules = modules;
		for (OutlineModuleElement module : modules.values()) {
			module.setParent(this);
		}
	}
	
	public boolean hasChildren() {
		if((modules == null) || (modules.isEmpty()))
			return false;
		return true;
	}

	public Object[] getElements() {
		return modules.values().toArray();
	}
	
	public void dispose() {
		modules.clear();
	}
}


