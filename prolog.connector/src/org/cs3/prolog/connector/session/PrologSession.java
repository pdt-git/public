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

package org.cs3.prolog.connector.session;

import java.util.List;
import java.util.Map;

import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologProcessException;

public interface PrologSession extends Disposable{
	
	/**
	 * See: {@link org.cs3.prolog.connector.process.PrologProcess#queryOnce(String...)}  
	 * @param query
	 * @return
	 * @throws PrologException
	 * @throws PrologProcessException
	 */
    public Map<String,Object> queryOnce(String query) throws PrologException, PrologProcessException;

	/**
	 * See: {@link org.cs3.prolog.connector.process.PrologProcess#queryAll(String...)}  
	 * @param query
	 * @return
	 * @throws PrologException
	 * @throws PrologProcessException
	 */
    public List<Map<String,Object>> queryAll(String query) throws PrologException, PrologProcessException;

	/**
	 * See: {@link org.cs3.prolog.connector.process.PrologProcess#queryAll(String...)}  
	 * @param query
	 * @param flag
	 * @return
	 * @throws PrologException
	 * @throws PrologProcessException
	 */
    public List<Map<String, Object>> queryAll(String query, int flag) throws PrologException, PrologProcessException;
	
}


