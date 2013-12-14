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
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.StyledString;
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
	private StyledString label;
	private String signature;
	
	private Boolean conversionSuccessful;
	private IDocument document;
	
	public PrologMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int line, String declOrDef, String signature) {
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
		this.signature = signature;
		initDocument();
		label = createLabel();
	}
	
	public PrologMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int offset, int length, String declOrDef, String signature) {
		super(searchMatchElement, UNIT_CHARACTER, offset, length);
		this.declOrDef = declOrDef;
		this.file = file;
		this.visibility = visibility;
		this.module = module;
		this.name = name;
		this.arity = arity;
		this.properties = properties;
		isLineLocation = false;
		this.signature = signature;
		initDocument();
		convertOffsetAndLength(offset, offset + length);
		parseLineFromProperties();
		label = createLabel();
	}
	
	protected void convertOffsetAndLength(int offset, int end) {
		try {
			long fileLength = EFS.getStore(file.getLocationURI()).fetchInfo().getLength();
			if (fileLength > 1024 * 1024) {
				conversionFailed();
			} else {
				IDocument document = getDocument();
				if (document == null) {
					conversionFailed();
				} else {
					int convertedOffset = UIUtils.logicalToPhysicalOffset(document, offset);
					setOffset(convertedOffset);
					int convertedEnd = UIUtils.logicalToPhysicalOffset(document, end);
					int length = convertedEnd - convertedOffset;
					setLength(length);
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
			conversionFailed();
		}
	}
	
	protected StyledString createLabel() {
		StyledString str;
		String labelProperty = PDTCommonUtil.getProperty(SearchConstants.PROPERTY_LABEL, properties);
		if (labelProperty != null) {
			str = new StyledString(Util.unquoteAtom(labelProperty));
		} else {
			str = new StyledString("");
		}
		return str;
	}
	
	private void conversionFailed() {
		conversionSuccessful = false;
		isLineLocation = true;
	}

	private void parseLineFromProperties() {
		String lineProperty = PDTCommonUtil.getProperty(SearchConstants.PROPERTY_LINE, properties);
		if (lineProperty == null) {
			line = 1;
		} else {
			try {
				line = Integer.parseInt(lineProperty);
				if (line < 1) {
					line = 1;
				}
			} catch (NumberFormatException e) {
				line = 1;
			}
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
		return "";
	}
	
	public String getSignature() {
		return signature;
	}

	public StyledString getStyledString() {
		return label;
	}
	
	public boolean conversionSuccessful() {
		if (conversionSuccessful != null) {
			return conversionSuccessful;
		} else {
			return isLineLocation;
		}
	}
	
	private void initDocument() {
		try {
			document = UIUtils.getDocument(file);
			if (document.getLength() <= 0) {
				Debug.warning("Empty document for file: " + file.getFullPath());
				document = null;
			}
		} catch (CoreException e) {
			Debug.report(e);
		}
	}
	
	public IDocument getDocument() {
		return document;
	}
	
}

