package org.cs3.pl.prolog.internal;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.rapla.components.rpc.MessagingClientLoggingWrapper;
import org.rapla.components.rpc.ServiceNotFoundException;
import org.rapla.components.rpc.TimeoutException;

/**
 * A reusable rpc client.
 * (Watchdog is disabled right now.)
 */
public class ReusableClient extends MessagingClientLoggingWrapper implements Reusable {
	
	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.Reusable#reuse()
	 */
	public void reuse() {
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.Reusable#destroy()
	 */
	public void destroy() {
		stop();
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.Reusable#recylce()
	 */
	public void recylce() {
		
	}
	public void start() throws IOException{
		super.start();
		//watchdog.start();
		
	}
	/* (non-Javadoc)
	 * @see org.rapla.components.rpc.MessagingClientImpl#call(java.lang.String, java.lang.String, java.lang.Object[])
	 */
	public Object call(String service, String methodName, Object[] arguments)
			throws InvocationTargetException, TimeoutException,
			ServiceNotFoundException, IOException {
	
		//watchdog.watch(3000);
		Object result = super.call(service, methodName, arguments);
		//watchdog.relax();
		return result;
	}
}
