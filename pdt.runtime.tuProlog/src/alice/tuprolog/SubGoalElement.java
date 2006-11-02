package alice.tuprolog;

public class SubGoalElement extends AbstractSubGoalTree {
	
	private Term term;
	
	public SubGoalElement(SubGoalTree parent, int pos, Term t) {
		super(parent,pos);
		term = t;
	}
	
	public Term getValue() {
		return term;
	}
	
	public boolean isLeaf() { return true; }
	public boolean isRoot() { return false; }
	
	
	public String toString() {
		return term.toString();
	}
}