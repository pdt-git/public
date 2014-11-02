package org.cs3.pdt.common.internal;

import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.common.PDTCommon;
import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PrologProcessStartListener;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.internal.service.ext.IPrologProcessServiceExtension;
import org.cs3.pdt.connector.service.ConsultListener;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;

public class ConsultManager implements ConsultListener, PrologProcessStartListener {

	@Override
	public void beforeConsult(PrologProcess process, List<IFile> files, IProgressMonitor monitor) throws PrologProcessException {
	}

	@Override
	public void afterConsult(PrologProcess process, List<IFile> files, List<String> allConsultedFiles, IProgressMonitor monitor) throws PrologProcessException {
		for (IFile file : files) {
			addConsultedFile(process, file);
		}
		monitor.done();
	}

	@Override
	public void prologProcessStarted(PrologProcess process) {
		final String reconsultFiles = PDTCommonPlugin.getDefault().getPreferenceValue(PDTCommon.PREF_RECONSULT_ON_RESTART, PDTCommon.RECONSULT_NONE);
		
		if (reconsultFiles.equals(PDTCommon.RECONSULT_NONE)) {
			getConsultedFileList(process).clear();
		} else {
			reconsultFiles(process, reconsultFiles.equals(PDTCommon.RECONSULT_ENTRY));
		}
	}
	
	// TODO: problem with quotes
	private void reconsultFiles(PrologProcess process, boolean onlyEntryPoints) {
		Debug.debug("Reconsult files");
		List<IFile> consultedFiles = getConsultedFileList(process);
		if (consultedFiles != null) {
			synchronized (consultedFiles) {
				ArrayList<IFile> files = new ArrayList<IFile>();
				ArrayList<IFile> entryPointFiles = new ArrayList<IFile>();
				IPrologProcessServiceExtension service = (IPrologProcessServiceExtension) PDTConnectorPlugin.getDefault().getPrologProcessService();
				if (onlyEntryPoints) {
					filterEntryPoints(files, entryPointFiles);
					service.consultFilesSilent(entryPointFiles, process);
				} else {
					service.consultFilesSilent(files, process);
				}
			}
		}
	}

	private List<IFile> getConsultedFileList(PrologProcess process) {
		@SuppressWarnings("unchecked")
		List<IFile> consultedFiles = (List<IFile>) process.getAttribute(PDTCommon.CONSULTED_FILES);
		return consultedFiles;
	}
	
	private void addConsultedFile(PrologProcess process, IFile file) {
		List<IFile> consultedFiles = getConsultedFileList(process);
		if (consultedFiles == null) {
			consultedFiles = new ArrayList<IFile>();
			process.setAttribute(PDTCommon.CONSULTED_FILES, consultedFiles);
		}
		synchronized (consultedFiles) {
			// only take the last consult of a file
			if (consultedFiles.remove(file)) {
				Debug.debug("move " + file + " to end of consulted files");			
			} else {
				Debug.debug("add " + file + " to consulted files");
			}
			consultedFiles.add(file);
		}
	}
	
	private void filterEntryPoints(List<IFile> files, List<IFile> entryPointFiles) {
		for (IFile file : files) {
			if (PDTCommonPlugin.getDefault().isEntryPoint(file)) {
				entryPointFiles.add(file);
			}
		}
	}

}
