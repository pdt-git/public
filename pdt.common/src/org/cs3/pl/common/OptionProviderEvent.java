package org.cs3.pl.common;

import java.util.EventObject;

public class OptionProviderEvent extends EventObject {

	public String id;

	public OptionProviderEvent(Object source,String id) {
		super(source);
		this.id=id;
	}

}
