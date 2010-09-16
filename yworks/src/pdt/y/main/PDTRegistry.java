package pdt.y.main;

import java.util.List;

import org.cs3.pdt.runtime.BootstrapPrologContribution;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.RegistryHook;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pdt.ui.util.EclipsePreferenceProvider;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;

public class PDTRegistry implements RegistryHook  {
	private static String NAME = "GraphML";

	public PDTRegistry() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void addSubscriptions(PrologInterfaceRegistry registry) {/*
		//PrologInterface prologInterface = (AbstractPrologInterface) AbstractPrologInterface.newInstance();
		//prologInterface.initOptions(new EclipsePreferenceProvider(PrologRuntimeUIPlugin.getDefault()));
		//registry.addPrologInterface(NAME, prologInterface);

		YWorksSubscription subscription = YWorksSubscription.newInstance(NAME);
		System.out.println("hier");
		registry.addSubscription(subscription);
		System.out.println("da");
		PrologInterface prologInterface = PrologRuntimeUIPlugin.getDefault().getPrologInterface(subscription);
		System.out.println(prologInterface);
		
//		List<String> contributionKeys = subscription.getBootstrapConstributionKeys(); 
		
		
	//	consultNeededPrologFiles(prologInterface, contributionKeys);
		
		
		//PrologInterface prologInterface = (AbstractPrologInterface) AbstractPrologInterface.newInstance();
		prologInterface.initOptions(new EclipsePreferenceProvider(PrologRuntimeUIPlugin.getDefault()));
		//registry.addPrologInterface(NAME, prologInterface);

		Activator.getDefault().setPrologInterface(prologInterface);*/
	}

	private void consultNeededPrologFiles(PrologInterface prologInterface,
			List<String> contributionKeys) {
		for (String contributionKey : contributionKeys) {
			List<BootstrapPrologContribution> libraryList = PrologRuntimePlugin.getDefault().getBootstrapList(contributionKey);
			for (BootstrapPrologContribution library : libraryList) {
				if (!prologInterface.getBootstrapLibraries().contains(library)) {
					prologInterface.getBootstrapLibraries().add(library);
					if (prologInterface.isUp()) {
						PrologSession session = null;
						try {
							session = prologInterface.getSession(PrologInterface.LEGACY);
							
							String consult = library.getPrologInitStatement();
							Debug.debug("consult " + consult + ", from " + library);
							session.queryOnce(consult);
						} catch (PrologInterfaceException e) {
							Debug.report(e);
							if (session != null)
								session.dispose();
						}
					}
				}
			}
		}
	}

}
