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

package org.cs3.pdt.common.structureElements;

import java.util.List;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.search.ui.text.Match;

public class PrologMatch extends Match{

	private String module;
	private int line = -1;
	private IFile file;

	private boolean isLineLocation= false;
	private String visibility;
	private String declOrDef;
	private String name;
	private int arity;
	private List<String> properties;
	private String label;
	
	public PrologMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int line, String declOrDef) {
		super(searchMatchElement, UNIT_LINE, line - 1, 1);
		this.declOrDef = declOrDef;
		this.file = file;
		this.visibility = visibility;
		this.module = module;
		this.name = name;
		this.arity = arity;
		this.properties = properties;
		this.line = line;
		isLineLocation = true;
	}
	
	public PrologMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int offset, int length, String declOrDef) {
		super(searchMatchElement, UNIT_CHARACTER, offset, length);
		this.declOrDef = declOrDef;
		this.file = file;
		this.visibility = visibility;
		this.module = module;
		this.name = name;
		this.arity = arity;
		this.properties = properties;
		isLineLocation = false;
		convertOffsetAndCreateLabel(offset, offset + length);
	}
	
	private void convertOffsetAndCreateLabel(int offset, int end) {
		try {
			long fileLength = EFS.getStore(file.getLocationURI()).fetchInfo().getLength();
			if (fileLength > 1024 * 1024) {
				isLineLocation = true;
				String lineProperty = PDTCommonUtil.getProperty("clause_line", properties);
				if (lineProperty == null) {
					line = 1;
				} else {
					try {
						line = Integer.parseInt(lineProperty);
					} catch (NumberFormatException e) {
						line = 1;
					}
				}
			} else {
				IDocument document;
				document = UIUtils.getDocument(file);
				if (document.getLength() <= 0) {
					Debug.warning("Empty document for file: " + file.getFullPath());
				} else {
					int convertedOffset = UIUtils.logicalToPhysicalOffset(document, offset);
					setOffset(convertedOffset);
					int convertedEnd = UIUtils.logicalToPhysicalOffset(document, end);
					int length = convertedEnd - convertedOffset;
					setLength(length);
					String text = document.get(convertedOffset, length);
					if (text != null) {
						String line = PDTCommonUtil.getProperty("line", properties);
						if (line != null) {
							label = line + ": " + text.replaceAll("\n|\r", "");
						} else {
							label = text.replaceAll("\n|\r", "");
						}
					}
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
		} catch (BadLocationException e) {
			Debug.report(e);
		}
	}

	public int getLine() {
		return line;
	}

	public void setLine(int line) {
		this.isLineLocation=true;
		this.line = line;
	}

	public boolean isLineLocation() {
		return isLineLocation;
	}

	public String getModule() {
		return module;
	}
	
	public String getName() {
		return name;
	}
	
	public int getArity() {
		return arity;
	}
	
	public List<String> getProperties() {
		return properties;
	}
	
	public String getDeclOrDef() {
		return declOrDef;
	}
	
	public String getVisibility() {
		return visibility;
	}
	
	public IFile getFile() {
		return file;
	}
	
	public String getLabel() {
		if (label == null) {
			String labelProperty = PDTCommonUtil.getProperty("label", properties);
			if (labelProperty != null) {
				label = labelProperty;
			} else {
				if (isLineLocation) {
//				String firstArgument = PDTCommonUtil.getProperty("first_argument", properties);
//				if (firstArgument != null) {
//					label = getLine() + ": " + Util.unquoteAtom(firstArgument);
//				} else {
					StringBuffer buf = new StringBuffer("Line ");
					buf.append(Integer.toString(getLine()));
					buf.append(" (");
					buf.append(declOrDef);
					buf.append(")");
					label = buf.toString();
//				}
				} else {
					label = Util.unquoteAtom(PDTCommonUtil.getProperty("goal", properties));
				}
			}
		}
		return label;
	}
	
	public void setLabel(String label) {
		this.label = label;
	}
	
}

