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

package org.cs3.pdt.internal.contentassistant;

import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;

public class VariableCompletionProposal extends ComparableCompletionProposal{
	
	public VariableCompletionProposal(String proposal, int begin, int len,
			int cursorPos, Image image, String displayString, IContextInformation contextInfo,
			String additionalInfo) {
		super(proposal,begin,len,cursorPos,image,displayString,contextInfo,additionalInfo);
	}

	@Override
	public int compareTo(ComparableCompletionProposal o) {
		if (o instanceof VariableCompletionProposal) {
			String displayString = target.getDisplayString();
			VariableCompletionProposal variableCompletionProposal = ((VariableCompletionProposal)o);
			CompletionProposal target2 = variableCompletionProposal.target;
			return displayString.compareTo(target2.getDisplayString());
		}
		return 0;
	}
}

