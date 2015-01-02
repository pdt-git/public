/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.analysis.model;

import java.util.TreeSet;

public class ResultGroup implements IResultElementGroup {
	
	private String analysisName;
	private String description;
	private IResultElement parent;
	private TreeSet<IResultElement> children = new TreeSet<>();

	public ResultGroup(String analysisName, String description, IResultElement parent) {
		this.analysisName = analysisName;
		this.description = description;
		this.parent = parent;
	}

	@Override
	public String getAnalysisName() {
		return analysisName;
	}

	@Override
	public String getDescription() {
		return description;
	}

	@Override
	public IResultElement getParent() {
		return parent;
	}

	@Override
	public IResultElement[] getChildren() {
		return children.toArray(new IResultElement[children.size()]);
	}

	@Override
	public boolean hasChildren() {
		return !children.isEmpty();
	}
	
	public void addChild(IResultElement element) {
		children.add(element);
	}

	@Override
	public int compareTo(IResultElement o) {
		int c = String.CASE_INSENSITIVE_ORDER.compare(analysisName, o.getAnalysisName());
		if (c != 0) {
			return c;
		}
		if (o instanceof IResultElementGroup) {
			return String.CASE_INSENSITIVE_ORDER.compare(description, o.getDescription());
		}
		return 0;
	}

}
