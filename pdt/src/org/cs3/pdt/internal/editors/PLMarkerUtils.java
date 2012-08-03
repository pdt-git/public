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

package org.cs3.pdt.internal.editors;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.PDTPredicates;
import org.cs3.pdt.quickfix.PDTMarker;
import org.cs3.prolog.common.FileUtils;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.PrologConnectorPredicates;
import org.cs3.prolog.pif.PrologException;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class PLMarkerUtils {

	private static int mapSeverity(String severity) {
		if ("error".equals(severity)) {
			return IMarker.SEVERITY_ERROR;
		}
		if ("warning".equals(severity)) {
			return IMarker.SEVERITY_WARNING;
		}
		if ("info".equals(severity)) {
			return IMarker.SEVERITY_INFO;
		}

		throw new IllegalArgumentException("cannot map severity constant: "
				+ severity);
	}

	public static void addMarkers(PrologInterface pif, List<String> allConsultedFiles, IProgressMonitor monitor) {
		monitor.beginTask("Update markers", 2);
		PrologSession session =null;
		try {
			session = pif.getSession();
			Map<String, IFile> reloadedFiles = new HashMap<String, IFile>();
			monitor.subTask("add markers for errors and warnings");
			collectIFilesForFileNames(allConsultedFiles, reloadedFiles);
			addMarkersForErrorsAndWarnings(session, new SubProgressMonitor(monitor, 1, SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK), reloadedFiles);
			monitor.subTask("Update Prolog Smells Detectors");
			addMarkersForSmellDetectors(session, new SubProgressMonitor(monitor, 1, SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK), reloadedFiles);
//			session.queryOnce("deactivate_warning_and_error_tracing");
		} catch (PrologException e) {
			// this may be a reload_timeout_reached exception
			// (shouldn't happen anymore, but maybe it does)

			// so at least we deactivate the tracing, because
			// otherwise error markers will still be visible after removing the error
//			try {
//				session.queryOnce("deactivate_warning_and_error_tracing");
//			} catch (Exception e1) {
//				Debug.report(e1);
//			}
		} catch (Exception e) {
			Debug.report(e);
		} finally {
			if (session != null) {
				session.dispose();
			}
			monitor.done();
		}
	}
	
	private static void addMarkersForErrorsAndWarnings(PrologSession session, SubProgressMonitor monitor, Map<String, IFile> reloadedFiles) throws PrologInterfaceException, CoreException {
		List<Map<String, Object>> msgs = session.queryAll(bT(PrologConnectorPredicates.ERRORS_AND_WARNINGS, "Kind", "Line", "Length", "Message", "File"));
		monitor.beginTask("add markers for errors and warnings", reloadedFiles.size() + msgs.size());
		
		for (IFile file : reloadedFiles.values()) {
			file.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
			monitor.worked(1);
		}
		
		for (Map<String, Object> msg : msgs) {
			int severity=0;
			try {
				severity = mapSeverity(((String)msg.get("Kind")));
			} catch(IllegalArgumentException e){
				monitor.worked(1);
				continue;
			}
			
			String fileName = msg.get("File").toString();
			IFile file = reloadedFiles.get(fileName);
			if (file == null) {
				monitor.worked(1);
				continue;
			}
			
			IMarker marker = file.createMarker(IMarker.PROBLEM);

			marker.setAttribute(IMarker.SEVERITY, severity);

			String msgText = msg.get("Message").toString();
			int line = Integer.parseInt(msg.get("Line").toString());
			if (severity == IMarker.SEVERITY_ERROR && msgText.startsWith("Exported procedure ") && msgText.endsWith(" is not defined\n")){
				line = 1;
			}
			
			MarkerUtilities.setLineNumber(marker, line);
			
			marker.setAttribute(IMarker.MESSAGE, msgText);
			monitor.worked(1);
		}
		monitor.done();
	}
	
	private static void addMarkersForSmellDetectors(PrologSession session, IProgressMonitor monitor, Map<String, IFile> reloadedFiles) throws PrologInterfaceException, CoreException {
		monitor.beginTask("Update Prolog Smells Detectors", reloadedFiles.size());

		for (String fileName : reloadedFiles.keySet()) {
			String query = bT(PDTPredicates.SMELL_MARKER_PDT, "Name", "Description", "QuickfixDescription", "QuickfixAction", Util.quoteAtom(fileName), "Start", "Length");
			List<Map<String, Object>> msgsSmells = session.queryAll(query);
			
			if(msgsSmells!=null) {
				IFile file = reloadedFiles.get(fileName);
				IDocument doc = Util.getDocument(file);
				for (Map<String, Object> msg : msgsSmells) {
					IMarker marker = file.createMarker(IMarker.PROBLEM);
					marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
					marker.setAttribute(PDTMarker.SMELL_NAME, msg.get("Name").toString());
					marker.setAttribute(PDTMarker.QUICKFIX_DESCRIPTION, msg.get("QuickfixDescription").toString());
					
					String msgText = (String)msg.get("Description");
					int startPl = Integer.parseInt(msg.get("Start").toString());
					int start = Util.logicalToPhysicalOffset(doc,startPl);
					int length = Integer.parseInt(msg.get("Length").toString());
					
					MarkerUtilities.setCharStart(marker, start);
					MarkerUtilities.setCharEnd(marker, start+length);
					
					marker.setAttribute(PDTMarker.QUICKFIX_ACTION, msg.get("QuickfixAction".toString()));
					marker.setAttribute(IMarker.MESSAGE, msgText);
				}
			}
			monitor.worked(1);
		}
		monitor.done();
	}
	
	private static void collectIFilesForFileNames(List<String> fileNames, Map<String, IFile> fileNameToIFiles) {
		for (String fileName : fileNames) {
			IFile file = null;
			try {
				file = FileUtils.findFileForLocation(fileName);
			} catch (IOException e1) {
				continue;
			} catch (IllegalArgumentException e2){
				continue;
			}
			if (file == null || !file.exists()){
				continue;
			}
			fileNameToIFiles.put(fileName, file);
		}
	}


}


