package org.cs3.prolog.connector.process;

import java.util.List;

public interface StartupStrategy {
	
	public List<String> getFileSearchPathInitStatements();
	public List<String> getLoadFileInitStatements();
	
}
