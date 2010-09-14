package pdt.y.main;

import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.RegistryHook;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;
import org.eclipse.jface.action.Action;

public class PDTRegistry implements RegistryHook  {

	public PDTRegistry() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void addSubscriptions(PrologInterfaceRegistry registry) {
		System.out.println("Registry: "+registry.getAllSubscriptionIDs());
		AbstractPrologInterface prologInterface = (AbstractPrologInterface) AbstractPrologInterface.newInstance();
//		prologInterface.addLifeCycleHook((LifeCycleHook) this, "GraphML", null);
		registry.addPrologInterface("GraphML", prologInterface);
//		registry.addSubscription((Subscription) this);
		System.out.println("After Creation: "+registry.getAllSubscriptionIDs());
		Activator.getDefault().setRegistry(registry);
	}

}
