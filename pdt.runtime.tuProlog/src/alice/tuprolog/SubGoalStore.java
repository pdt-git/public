package alice.tuprolog;


public class SubGoalStore {
	
	private SubGoalTree goals;
	
	private SubGoalTree currentRoot;
	private int index;
	
	private SubGoalId tempId;
	
	
	public SubGoalStore() {
		goals = new SubGoalTree();
		currentRoot = goals;
		index = 0;
		tempId = null;
	}
	
	
	/**
	 * 
	 */
	public boolean load(SubGoalTree subGoals) {
		goals = subGoals;
		currentRoot = goals;
		return true;
	}
	
	
	public void pushSubGoal(SubGoalTree subGoals) {
		subGoals.setParentPosition(currentRoot,index);
		tempId = new DefaultSubGoalId(currentRoot,index);
		currentRoot = subGoals;
		index = 0;
	}
	
	
	/**
	 * Restituisce la clausola da caricare
	 */
	public Term fetch() {
		if (index >= currentRoot.size()) {
			SubGoalTree r = currentRoot.getParent();
			if (r == null) {
				return null;
			} else {
				tempId = new DefaultSubGoalId(currentRoot,index);
				index = currentRoot.getPosition() + 1;
				currentRoot = r;
				return fetch();
			}
		} else {
			AbstractSubGoalTree s = currentRoot.getChild(index);
			if (s.isLeaf()) {
				SubGoalElement l = (SubGoalElement)s;
				tempId = new DefaultSubGoalId(currentRoot,index);
				index++;
				return l.getValue();
			} else {
				SubGoalTree r = (SubGoalTree)s;
				tempId = new DefaultSubGoalId(currentRoot,index);
				currentRoot = r;
				index = 0;
				return fetch();
			}
		}		
	}
	
	/**
	 * Indice del correntemente in esecuzione
	 */
	public SubGoalId getCurrentGoalId() {
		return tempId;
	}
	
	
	/**
	 * Ripristina ClauseStore allo stato i-esimo
	 */
	public Term backTo(SubGoalId identifier) {
		DefaultSubGoalId id = (DefaultSubGoalId)identifier;
		currentRoot = id.getRoot();
		index = id.getIndex();
		return fetch();
	}
	
	
	public boolean haveSubGoals() {
		return index <= goals.size();
	}
	
	
	public String toString() {
		return "goals: "+goals+" "+
		"index: "+index;
	}
	
	
	/*
	 * Methods for spyListeners
	 */
	
	public SubGoalTree getSubGoals() {
		return goals;
	}
	
	public int getIndexNextSubGoal() {
		return index;
	}
	
}