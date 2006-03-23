package org.cs3.pl.prolog;

import java.util.EventObject;
import java.util.Map;

public class AsyncPrologSessionEvent extends EventObject {
	
	private static final long serialVersionUID = 1787537795491818031L;
	
	public Object ticket=null;
	public String message=null;
	public Map bindings = null;
	
	public AsyncPrologSessionEvent(Object source) {
		super(source);

	}

	public AsyncPrologSessionEvent(Object source, Object ticket) {
		super(source);
		this.ticket=ticket;
	}

	public AsyncPrologSessionEvent(Object source, Object ticket, String message) {
		super(source);
		this.ticket=ticket;
		this.message=message;
	}

	public AsyncPrologSessionEvent(Object source, Object ticket, Map bindings) {
		super(source);
		this.ticket=ticket;
		this.bindings=bindings;
	}

}
