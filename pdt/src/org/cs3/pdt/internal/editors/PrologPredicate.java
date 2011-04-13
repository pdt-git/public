package org.cs3.pdt.internal.editors;

public class PrologPredicate {
	String name;
	int arity;
	int line;
	String file;
	boolean isPublic;
	boolean dynamic;
	boolean multifile;
	
	@Override
	public String toString() {
		return name + "/" + arity;
	}

	public boolean isPublic() {
		return isPublic;
	}

	public void setPublic(boolean isPublic) {
		this.isPublic = isPublic;
		
	}

	public void setMultifile(boolean multifile) {
		this.multifile = multifile;
		
	}

	public void setDynamic(boolean dynamic) {
		this.dynamic = dynamic;
	}
}

