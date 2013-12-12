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

package org.cs3.pdt;


import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

public final class PDTUtils {

	public static PrologInterface getActivePif() {
		return PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface();
	}
	
	public static String getPrologFileName(IFile file) {
		String enclFile = file.getRawLocation().toPortableString();
		if (Util.isWindows()) {
			enclFile = enclFile.toLowerCase();
		}

		IPath filepath = new Path(enclFile);
		return Util.prologFileName(filepath.toFile());
	}

	private static String plDocCss;
	
	public static String getPlDocCss() {
		if (plDocCss == null) {
			URL url = PDTPlugin.getDefault().getBundle().getEntry("/css/pldoc.css");
			StringBuilder buf = new StringBuilder();
			try {
				InputStream inputStream = url.openConnection().getInputStream();
				BufferedReader in = new BufferedReader(new InputStreamReader(inputStream));
				String inputLine;
				while ((inputLine = in.readLine()) != null) {
					buf.append(inputLine);
				}
				in.close();
			} catch (IOException e) {
				Debug.report(e);
			}
			plDocCss = buf.toString();
		}
		return plDocCss;
	}

}


