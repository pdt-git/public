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
		setLineKey("Line");
		setArityKey("Arity");
		setFunctorKey("Name");
		setModuleKey("RefModule");
		setFileKey("File");
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
		
/*		if(clauses.size()==0){ 
			// a user module predicate (e.g. clause_property/2) is not-yet used in a module:
			query = "get_references(_,'" + goal.getName()+"'/" + goal.getArity()+ "," + module  + ",File,Line,RefModule,Name,Arity)";
			Debug.info("Look up predicate in user module: "+query); 
			clauses = session.queryAll(query);
		} 
		if(clauses.size()>0 && goal.getModule()==null){
			goal.setModule((String)clauses.get(0).get("Module"));
		}
*/		return clauses;
	}
}
