package org.cs3.pl.parser;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Set;


public interface PrologCompiler {

	public static final String METADATA = "meta_data";

	public static final String METADATAHELP = METADATA + "_help";

	public static final String METADATAMODULE = METADATA + "_module";

	public static final String MSG_COND_PAREN = "the condition part of the ct should be surrounded by parenthesis.";

	public static final String MSG_ACTION_PAREN = "the action part of the ct should be surrounded by parenthesis.";

	public static final String MSG_SINGLETON_VAR_PREFIX = "Singleton variable: ";

	public static final String MSG_SINGLETON_VAR_POSTFIX = " (one occurence only)";

	public abstract void compile(String content);

	public abstract void compile(String symbolicFileName, InputStream content,
			LineBreakInfoProvider lineInfo);
	
	
	public ProblemCollector getProblemCollector();
	public void setProblemCollector(ProblemCollector problemCollector);
	
	public TaskCollector getTaskCollector();
	public void setTaskCollector(TaskCollector taskCollector);
	
	
	public abstract void saveMetaDataForClauses(OutputStream stream)
			throws IOException;

	
	/**@deprecated currently only used by test cases, will be removed.*/
	public abstract List getClauses();
	/**@deprecated currently only used by test cases, will be removed.*/
	public abstract String getModuleName();
	/**@deprecated currently only used by test cases, will be removed.*/
	public abstract Set getPublicModulePredicates();

	public abstract void saveAbbaData(OutputStream out) throws IOException;

	public abstract void updateIndex(Index index);		
	

}