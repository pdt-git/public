package org.cs3.prolog.pif;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.cs3.prolog.common.Util;

public class DefaultStartupStrategy implements StartupStrategy {
	
	private final List<String> loadFileInitStatments = new ArrayList<>();
	private final List<String> fileSearchPathInitStatements = new ArrayList<>(); 

	public void addLoadFile(File loadFile) {
		String cmd = createConsultCommand(loadFile);
		loadFileInitStatments.add(cmd);
	}
	
	public boolean removeLoadFile(File loadFile) {
		String cmd = createConsultCommand(loadFile);
		return loadFileInitStatments.remove(cmd);
	}
	
	private String createConsultCommand(File loadFile) {
		return "['" + Util.prologFileName(loadFile) + "']";
	}

	public void addFileSearchPath(String alias, File fsp) {
		String cmd = createFileSearchPathCommand(alias, fsp);
		fileSearchPathInitStatements.add(cmd);
	}
	
	public boolean removeFileSearchPath(String alias, File fsp) {
		String cmd = createFileSearchPathCommand(alias, fsp);
		return fileSearchPathInitStatements.remove(cmd);
	}
	
	private String createFileSearchPathCommand(String alias, File fsp) {
		String term = bT("user:file_search_path", alias, Util.quoteAtom(Util.prologFileName(fsp)));
		return bT("assertz", term);
	}

	@Override
	public List<String> getFileSearchPathInitStatements() {
		return fileSearchPathInitStatements;
	}

	@Override
	public List<String> getLoadFileInitStatements() {
		return loadFileInitStatments;
	}

}
