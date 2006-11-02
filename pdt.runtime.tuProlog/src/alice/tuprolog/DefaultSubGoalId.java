package alice.tuprolog;

/**
 * Identifier of single subGoal during the demo.
 * @author Alex Benini
 *
 */
public class DefaultSubGoalId implements SubGoalId {
	
	private SubGoalTree root;
	private int index;
	
	DefaultSubGoalId(SubGoalTree root, int index) {
		this.root = root;
		this.index = index;
	}
	
	SubGoalTree getRoot() {
		return root;
	}
	
	int getIndex() {
		return index;
	}
	
	public String toString() {
		return root.getChild(index).toString();
	}

}