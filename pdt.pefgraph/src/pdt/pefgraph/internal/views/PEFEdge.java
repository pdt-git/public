package pdt.pefgraph.internal.views;

public class PEFEdge {
	public String from;
	public String label;
	public String to;
	public PEFEdge(String from, String label, String to) {
		this.from = from;
		this.label = label;
		this.to = to;
	}
	@Override
	public boolean equals(Object obj) {
		if(!(obj instanceof PEFEdge)){
			return false;
		}
		PEFEdge other = (PEFEdge) obj; 
		return from.equals(other.from) && to.equals(other.to)&& label.equals(other.label);
				
	}
	@Override
	public int hashCode() {
	
		return (from+label+to).hashCode();
	}
}
