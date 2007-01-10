package alice.tuprolog;


public abstract class AbstractSubGoalTree {
	
	private SubGoalTree parent;
	private int position;
	
	AbstractSubGoalTree(SubGoalTree parent, int position) {
		this.parent = parent;
		this.position = position;
	}
	
	public boolean setParentPosition(SubGoalTree parent, int position) {
		if (this.parent == null) {
			this.parent = parent;
			this.position = position;
			return true;
		}
		return false;
	}
	
	public SubGoalTree getParent() {
		return parent;
	}
	
	public int getPosition() {
		return position;
	}
	
	public abstract boolean isLeaf();
	
	public abstract boolean isRoot();
	
}