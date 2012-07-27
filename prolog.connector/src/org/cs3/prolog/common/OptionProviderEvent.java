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

package org.cs3.prolog.common;

import java.util.EventObject;

public class OptionProviderEvent extends EventObject {

	private static final long serialVersionUID = 1L;
	public String[] ids;

	public OptionProviderEvent(Object source,String id) {
		super(source);
		this.ids=new String[]{id};
	}

	public OptionProviderEvent(Object source, String[] ids) {
		super(source);
		this.ids=ids;
	}

}


