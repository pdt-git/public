package org.cs3.pdt.console.internal.views;

public class Completion implements Comparable<Completion> {

	private String functor;
	private int arity;
	
	public Completion(String functor, int arity) {
		this.functor = functor;
		this.arity = arity;
	}
	
	public String getFunctor() {
		return functor;
	}
	
	public int getArity() {
		return arity;
	}

	public String getArglist() {
		if (arity < 1) {
			return "";
		}
		
		StringBuffer buf = new StringBuffer("(");
		for (int i=0; i<arity; i++) {
			if(i > 0) {
				buf.append(", ");
			}
			buf.append("_");
		}
		buf.append(")");
		return buf.toString();
	}

	@Override
	public int compareTo(Completion o) {
		return functor.compareTo(o.getFunctor());
	}
}
