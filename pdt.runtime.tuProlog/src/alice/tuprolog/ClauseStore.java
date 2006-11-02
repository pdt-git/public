package alice.tuprolog;

import java.util.*;

import alice.util.OneWayList;

/**
 * A list of clauses belonging to the same family as a goal. A family is
 * composed by clauses with the same functor and arity.
 */
public class ClauseStore {
	
	private OneWayList clauses;
	private Term goal;
	private List vars;
	private boolean haveAlternatives;
	
	
	private ClauseStore(Term goal, List vars) {
		this.goal = goal;
		this.vars = vars;
		clauses = null;
	}
	
	
	/**
	 * Carica una famiglia di clausole
	 * @param familyClauses
	 */
	public static ClauseStore build(Term goal, List vars, List familyClauses) {
		ClauseStore clauseStore = new ClauseStore(goal, vars);
		clauseStore.clauses = OneWayList.transform(familyClauses);
		if (clauseStore.clauses == null || !clauseStore.existCompatibleClause())
			return null;
		return clauseStore;
	}
	
	
	/**
	 * Restituisce la clausola da caricare
	 */
	public ClauseInfo fetch() {
		if (clauses == null) return null;
		deunify(vars);
		if (!checkCompatibility(goal))
			return null;
		ClauseInfo clause = (ClauseInfo) clauses.getHead();
		clauses = clauses.getTail();
		haveAlternatives = checkCompatibility(goal);
		return clause;
	}
	
	
	public boolean haveAlternatives() {
		return haveAlternatives;
	}
	
	
	/**
	 * Verify if there is a term in compatibleGoals compatible with goal. 
	 * @param goal
	 * @param compGoals
	 * @return true if compatible or false otherwise.
	 */
	protected boolean existCompatibleClause() {
		List saveUnifications = deunify(vars);
		boolean found = checkCompatibility(goal);
		reunify(vars, saveUnifications);
		return found;
	}
	
	
	/**
	 * Salva le unificazioni delle variabili da deunificare
	 * @param varsToDeunify
	 * @return unificazioni delle variabili
	 */
	private List deunify(List varsToDeunify) {
		List saveUnifications = new ArrayList();
		//deunifico le variabili termporaneamente
		Iterator it = varsToDeunify.iterator();
		while (it.hasNext()) {
			Var v = ((Var) it.next());
			saveUnifications.add(v.getLink());
			v.free();
		}
		return saveUnifications;
	}
	
	
	/**
	 * Restore previous unifications into variables.
	 * @param varsToReunify
	 * @param saveUnifications
	 */
	private void reunify(List varsToReunify, List saveUnifications) {
		int size = varsToReunify.size();
		ListIterator it1 = varsToReunify.listIterator(size);
		ListIterator it2 = saveUnifications.listIterator(size);
		// Only the first occurrence of a variable gets its binding saved;
		// following occurrences get a null instead. So, to avoid clashes
		// between those values, and avoid random variable deunification,
		// the reunification is made starting from the end of the list.
		while (it1.hasPrevious()) {
			((Var) it1.previous()).setLink((Term) it2.previous());
		}
	}
	
	
	/**
	 * Verify if a clause exists that is compatible with goal.
	 * As a side effect, clauses that are not compatible get
	 * discarded from the currently examined family.
	 * @param goal
	 */
	private boolean checkCompatibility(Term goal) {
		if (clauses == null) return false;
		ClauseInfo clause = null;
		do {
			clause = (ClauseInfo) clauses.getHead();
			if (goal.match(clause.getHead())) return true;
			clauses = clauses.getTail();
		} while (clauses != null);
		return false;
	}
	
	
	public String toString() {
		return "clauses: "+clauses+"\n"+
		"goal: "+goal+"\n"+
		"vars: "+vars+"\n";
	}
	
	
	/*
	 * Methods for spyListeners
	 */
	
	public List getClauses() {
		ArrayList l = new ArrayList();
		OneWayList t = clauses;
		while (t != null) {
			l.add(t.getHead());
			t = t.getTail();
		}
		return l;
	}
	
	public Term getMatchGoal() {
		return goal;
	}
	
	public List getVarsForMatch() {
		return vars;
	}
	
	
}