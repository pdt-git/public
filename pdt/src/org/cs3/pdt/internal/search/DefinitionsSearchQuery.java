package org.cs3.pdt.internal.search;

import java.util.List;
import java.util.Map;

import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.GoalData;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;

public class DefinitionsSearchQuery extends PrologSearchQuery {

	public DefinitionsSearchQuery(PrologInterface pif, GoalData goal) {
		super(pif, goal);
		setModuleKey("Module");
		setFileKey("File");
		setLineKey("Line");
		setFunctorKey("Name");
		setArityKey("Arity");
	}
	
	
	@Override
	protected String buildSearchQuery(String module, String enclFile, GoalData goal) {
		String query = "find_definition_visible_in('"+enclFile+"','" + goal.getName()+"'," + goal.getArity()+ "," + module  + "," +
						getFileKey() + "," + 
						getLineKey() + "," +
						getFunctorKey() + "," +
						getArityKey() + ")";
		return query;
	}

	@Override
	protected List<Map<String, Object>> getResultForQuery(PrologSession session,
			String module, String query, GoalData goal) throws PrologInterfaceException {
		Debug.info(query);
		
		List<Map<String, Object>> clauses = session.queryAll(query);
		return clauses;
	}
}
