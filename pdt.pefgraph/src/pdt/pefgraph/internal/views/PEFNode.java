package pdt.pefgraph.internal.views;

import org.cs3.pdt.core.PEFHandle;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.Platform;

public class PEFNode implements PEFHandle{
	private String id;
	private String type;
	private String[] labels;
	private PrologInterface pif;
	public PEFNode(String id, String type, String[] labels, PrologInterface pif) {
		super();
		this.id = id;
		this.type = type;
		this.labels = labels;
		this.pif = pif;
	}

	@Override
	public boolean equals(Object obj) {
		if(!(obj instanceof PEFNode)){
			return false;
		}
		PEFNode node = (PEFNode)obj;
		return id.equals(node.id);
	}
	
	@Override
	public int hashCode() {	
		return id.hashCode();
	}

	public String getId() {		
		return id;
	}

	public PrologInterface getPrologInterface() {
		return pif;
	}

	public String getType() {
		return type;
	}
	
	public String[] getLabels(){
		return labels;
	}
	public Object getAdapter(Class adapter) {
		return Platform.getAdapterManager().getAdapter(this,
				adapter);
	}
}
