package org.cs3.pl.prolog.tests;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook3;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologSession;

import junit.framework.TestCase;

public class LifeCycleHookTest extends TestCase {


	private class MyHook implements LifeCycleHook3{

		private int lateInit;
		private int onError;
		private int setData;
		private Object data;
		private int afterInit;
		private int beforeShutdown;
		private int onInit;

		public void lateInit(PrologInterface pif) {
			Debug.debug("lateInit");
			lateInit++;
			
		}

		public void onError(PrologInterface pif) {
			Debug.debug("onError");
			onError++;
			
		}

		public void setData(Object data) {
			Debug.debug("setData");
			this.data=data;
			setData++;
			
		}

		public void afterInit(PrologInterface pif)
				throws PrologInterfaceException {
			Debug.debug("afterInit");
			afterInit++;
			
		}

		public void beforeShutdown(PrologInterface pif, PrologSession session)
				throws PrologInterfaceException {
			Debug.debug("beforeShutdown");
			beforeShutdown++;
			
		}

		public void onInit(PrologInterface pif, PrologSession initSession)
				throws PrologInterfaceException {
			Debug.debug("onInit");
			onInit++;
			
		}
		
	}
	
	
	private PrologInterface2 pif;

	protected void setUp() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		this.pif=(PrologInterface2) PrologInterfaceFactory.newInstance().create();
		
	}

	public void testPDT_295_00() throws Exception{
		MyHook X = new MyHook();
		pif.addLifeCycleHook(X, "X", new String[0]);
		pif.getSession(PrologInterface.NONE).dispose();
		pif.stop();
		assertEquals(1,X.onInit);
		assertEquals(1,X.afterInit);
		assertEquals(1,X.beforeShutdown);
		/*
		 * -Dpif.file_search_path="library=/home/lukas/workspace/pdt.runtime.pifcom/library/pifcom"
-Dpif.implementation="org.cs3.pifcom.Factory"
		 */
		
	}
	
	public void testPDT_295_01() throws Exception{
		MyHook X = new MyHook();
		pif.addLifeCycleHook(X, "X", new String[0]);
		pif.removeLifeCycleHook(X,"X");
		pif.addLifeCycleHook(X, "X", new String[0]);
		pif.getSession(PrologInterface.NONE).dispose();
		pif.stop();
		assertEquals(1,X.onInit);
		assertEquals(1,X.afterInit);
		assertEquals(1,X.beforeShutdown);
		/*
		 * -Dpif.file_search_path="library=/home/lukas/workspace/pdt.runtime.pifcom/library/pifcom"
-Dpif.implementation="org.cs3.pifcom.Factory"
		 */
		
	}
}
