package org.cs3.prolog.pif.service;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;
import java.util.List;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;

public class DefaultReloadExecutor implements PDTReloadExecutor {
	
	@Override
	public int getPriority() {
		return 0;
	}
	
	@Override
	public boolean executePDTReload(PrologInterface pif, IFile file, IProgressMonitor monitor) throws PrologInterfaceException {
		monitor.beginTask("", 1);
		try {
			pif.queryOnce(bT("pdt_reload", Util.quoteAtom(Util.prologFileName(file))));
			return true;
		} catch (IOException e) {
			Debug.report(e);
			return false;
		} finally {
			monitor.done();
		}
	}

	@Override
	public boolean executePDTReload(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
		monitor.beginTask("", 1);
		if (files.isEmpty()) {
			monitor.done();
			return true;
		}
		
		boolean first = true;
		StringBuffer buffer = new StringBuffer("[");
		for (IFile f : files) {
			if (first) {
				first = false;
			} else {
				buffer.append(", ");
			}
			try {
				buffer.append(Util.quoteAtom(Util.prologFileName(f)));
			} catch (IOException e) {
				Debug.report(e);
				return false;
			}
		};
		buffer.append("]");
		pif.queryOnce(bT("pdt_reload", buffer.toString()));
		monitor.done();
		return true;
	}
	
}
