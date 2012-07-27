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

package org.cs3.prolog.connector.ui;

import java.util.EventObject;

import org.cs3.prolog.pif.PrologInterface;

public class PrologContextTrackerEvent extends EventObject {


	private static final long serialVersionUID = 1L;
	private PrologInterface pif;

	/**
	 * creates a new PrologContextTrackerEvent.
	 * 
	 * 
	 * @param source
	 *            this should be the tracker that caused the event.
	 * @param pif
	 *            this should be what the pif thinks is the currently active
	 *            PrologInterface _AFTER_ the change. Maybe null to indicate
	 *            that no pif is currently active according to the source
	 *            tracker.
	 */
	public PrologContextTrackerEvent(Object source, PrologInterface pif) {
		super(source);
		this.pif = pif;
	}

	/**
	 * @return the currently active PrologInterface (according to the tracker
	 *         that send this event), or null if no PrologInterface is active
	 *         (dito).
	 */
	public PrologInterface getPrologInterface() {
		return pif;
	}
}


