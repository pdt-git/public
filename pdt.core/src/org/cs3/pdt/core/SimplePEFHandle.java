package org.cs3.pdt.core;

import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.Platform;

public class SimplePEFHandle implements PEFHandle {

	private String id;
	private PrologInterface pif;

	public String getId() {

		return id;
	}

	public SimplePEFHandle(PrologInterface pif, String id) {
		super();
		this.pif = pif;
		this.id = id;
	}

	public PrologInterface getPrologInterface() {

		return pif;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object getAdapter(Class adapter) {
		return Platform.getAdapterManager().getAdapter(this, adapter);
	}

}
