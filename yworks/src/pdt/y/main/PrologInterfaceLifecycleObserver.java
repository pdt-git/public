package pdt.y.main;

import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;


public class PrologInterfaceLifecycleObserver implements LifeCycleHook {

	public PrologInterfaceLifecycleObserver() {
	}

	@Override
	public void afterInit(PrologInterface pif) throws PrologInterfaceException {
		System.out.println("Here my pif: "+pif.toString());
	}

	@Override
	public void beforeShutdown(PrologInterface pif, PrologSession session)
			throws PrologInterfaceException {
	}

	@Override
	public void lateInit(PrologInterface pif) {
	}

	@Override
	public void onError(PrologInterface pif) {
	}

	@Override
	public void onInit(PrologInterface pif, PrologSession initSession)
			throws PrologInterfaceException {
	}

	@Override
	public void setData(Object data) {
	}

}
