package org.cs3.pdt.internal.search;

import java.util.List;
import java.util.Map;

import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.GoalData;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;

public class OldReferencesSearchQuery extends PrologSearchQuery {

	

	public OldReferencesSearchQuery(PrologInterface pif, GoalData goal) {
		super(pif, goal);
		setLineKey("Line");
		setArityKey("Arity");
		setFunctorKey("Name");
		setModuleKey("RefModule");
		setFileKey("File");
	}
	
	protected String buildSearchQuery(String module, String enclFile, GoalData goal) {
		String query = "get_references('"+enclFile+"','" + goal.getName()+"'/" + goal.getArity()+ "," + module  + "," +
						getFileKey() + "," +
						getLineKey() + "," +
						getModuleKey() + "," +
						getFunctorKey() + "," +
						getArityKey() + ")";
		return query;
	}


	protected List<Map<String, Object>> getResultForQuery(PrologSession session,
			String module, String query, GoalData goal) throws PrologInterfaceException {
		Debug.info(query);
		
		List<Map<String, Object>> clauses = session.queryAll(query);
		
		if(clauses.size()==0){ 
			// a user module predicate (e.g. clause_property/2) is not-yet used in a module:
			query = "get_references(_,'" + goal.getName()+"'/" + goal.getArity()+ "," + module  + "," +
						getFileKey() + "," +
						getLineKey() + "," +
						getModuleKey() + "," +
						getFunctorKey() + "," +
						getArityKey() + ")";
			Debug.info("Look up predicate in user module: "+query); 
			clauses = session.queryAll(query);
		} 
		if(clauses.size()>0 && goal.getModule()==null){
			goal.setModule((String)clauses.get(0).get("Module"));
		}
		return clauses;
	}
	
}
