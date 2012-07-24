/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.session;

import java.util.EventObject;
import java.util.Map;

public class AsyncPrologSessionEvent extends EventObject {
	
	private static final long serialVersionUID = 1787537795491818031L;
	public String query=null;
	public Object ticket=null;
	public String message=null;
	private Map<String, Object> bindings = null;
	public Exception exception=null;
	public int id=-1;
	
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

	public AsyncPrologSessionEvent(Object source, Object ticket, Map<String, Object> bindings) {
		super(source);
		this.ticket=ticket;
		this.bindings=bindings;
	}

	public Map<String, Object> getBindings() {
		return bindings;
	}

}

