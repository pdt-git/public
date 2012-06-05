package org.cs3.pl.common;

import java.util.EventObject;

public class OptionProviderEvent extends EventObject {

	private static final long serialVersionUID = 1L;
	public String[] ids;

	public OptionProviderEvent(Object source,String id) {
		super(source);
		this.ids=new String[]{id};
	}

	public OptionProviderEvent(Object source, String[] ids) {
		super(source);
		this.ids=ids;
	}

}
