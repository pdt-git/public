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

package org.cs3.pl.metadata.internal.classic;

import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.MetaInfoProviderFactory;
import org.cs3.pl.prolog.PrologInterface;


public class Factory extends MetaInfoProviderFactory {

	@Override
	public IMetaInfoProvider create(PrologInterface pif) {
		return new DefaultMetaInfoProvider(pif);
	}

}


