package org.cs3.pdt.common.internal;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.common.PDTCommon;
import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PrologInterfaceStartListener;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.internal.service.ext.IPrologInterfaceServiceExtension;
import org.cs3.pdt.connector.service.ConsultListener;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.prolog.connector.common.logging.Debug;
import org.cs3.prolog.connector.process.PrologInterface;
import org.cs3.prolog.connector.process.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.QualifiedName;

public class ConsultManager implements ConsultListener, PrologInterfaceStartListener {

	@Override
	public void beforeConsult(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
	}

	@Override
	public void afterConsult(PrologInterface pif, List<IFile> files, List<String> allConsultedFiles, IProgressMonitor monitor) throws PrologInterfaceException {
		for (IFile file : files) {
			try {
				String prologFileName = UIUtils.prologFileName(file);
				pif.addConsultedFile(prologFileName);
			} catch (IOException e) {
				Debug.report(e);
			}
		}
		monitor.done();
	}

	@Override
	public void prologInterfaceStarted(PrologInterface pif) {
		final String reconsultFiles = PDTCommonPlugin.getDefault().getPreferenceValue(PDTCommon.PREF_RECONSULT_ON_RESTART, PDTCommon.RECONSULT_NONE);
		
		if (reconsultFiles.equals(PDTCommon.RECONSULT_NONE)) {
			pif.clearConsultedFiles();
		} else {
			reconsultFiles(pif, reconsultFiles.equals(PDTCommon.RECONSULT_ENTRY));
		}
	}
	
	// TODO: problem with quotes
	private void reconsultFiles(PrologInterface pif, boolean onlyEntryPoints) {
		Debug.debug("Reconsult files");
		List<String> consultedFiles = pif.getConsultedFiles();
		if (consultedFiles != null) {
			synchronized (consultedFiles) {
				
				ArrayList<IFile> files = new ArrayList<IFile>();
				ArrayList<IFile> entryPointFiles = new ArrayList<IFile>();
				collectFiles(consultedFiles, files);
				IPrologInterfaceServiceExtension service = (IPrologInterfaceServiceExtension) PDTConnectorPlugin.getDefault().getPrologInterfaceService();
				if (onlyEntryPoints) {
					filterEntryPoints(files, entryPointFiles);
					service.consultFilesSilent(entryPointFiles, pif);
				} else {
					service.consultFilesSilent(files, pif);
				}
			}
		}
	}
	
	private void collectFiles(List<String> consultedFiles, List<IFile> files) {
		for (String consultedFile : consultedFiles) {
			IFile file;
			try {
				file = FileUtils.findFileForLocation(consultedFile);
				if (file != null){
					files.add(file);
				}
			} catch (IOException e) {
				Debug.report(e);
			}
		}
	}
	
	private void filterEntryPoints(List<IFile> files, List<IFile> entryPointFiles) {
		for (IFile file : files) {
			try {
				String isEntryPoint = file.getPersistentProperty(new QualifiedName("pdt", "entry.point"));
				if (isEntryPoint != null && isEntryPoint.equalsIgnoreCase("true")) {
					entryPointFiles.add(file);
				}
			} catch (CoreException e) {
				Debug.report(e);
			}
		}
	}

}
