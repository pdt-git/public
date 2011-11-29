package pdt.y.main;

import java.io.File;
import java.net.MalformedURLException;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;

public class GraphPIFCoordinator {
	private static final String FILE_TO_CONSULT = "pl_ast_to_graphML";
	private static final String PATH_ALIAS = "pdt_runtime_builder_graphml_creator";
	private static final String NAME_OF_HELPING_FILE = "pdt-focus-help.graphml";

	private File helpFile;
	private PDTGraphSwingStandalone view;
	private PrologInterface pif;
	private ExecutorService executor = Executors.newCachedThreadPool();

	public GraphPIFCoordinator(PDTGraphSwingStandalone view) {
		this.view = view;
		PrologRuntimeUIPlugin plugin = PrologRuntimeUIPlugin.getDefault();
		ResourceFileLocator locator = plugin.getResourceLocator();
		helpFile = locator.resolve(NAME_OF_HELPING_FILE);
	}

	public void queryPrologForGraphFacts(String focusFileForParsing) {

		String prologNameOfFileToConsult = PATH_ALIAS + "(" + FILE_TO_CONSULT
				+ ")";

		try {
			PrologConsole activeConsole = PrologConsolePlugin.getDefault()
					.getPrologConsoleService().getActivePrologConsole();
			
			if (activeConsole != null) {
				pif = getPifForActiveConsole(activeConsole);

				String query = "consult(" + prologNameOfFileToConsult + ").";
				sendQueryToCurrentPiF(query);

				query = "write_focus_to_graphML('" + focusFileForParsing
						+ "','" + Util.prologFileName(helpFile) + "').";
				sendQueryToCurrentPiF(query);

				// query =
				// "collect_ids_for_focus_file(FocusId,Files,CalledPredicates,Calls)";
				// Map<String, Object> result = sendQueryToCurrentPiF(query);
				// result.get("FocusId");

				FutureTask<?> futureTask = new FutureTask<Object>(
						new Runnable() {
							@Override
							public void run() {
								try {
									view.loadGraph(helpFile.toURI().toURL());
								} catch (MalformedURLException e) {
									Debug.rethrow(e);
								}
							};
						}, null);
				executor.execute(futureTask);
			}
		} catch (PrologException e1) {
			e1.printStackTrace();
		} catch (PrologInterfaceException e) {
			e.printStackTrace();
		}
	}

	public Map<String, Object> sendQueryToCurrentPiF(String query)
			throws PrologInterfaceException {

		PrologSession session = pif.getSession(PrologInterface.LEGACY);
		Map<String, Object> result = session.queryOnce(query);
		return result;
	}

	public PrologInterface getPifForActiveConsole(PrologConsole activeConsole) {
		PrologInterface pif = activeConsole.getPrologInterface();
		PrologInterfaceRegistry pifRegistry = PrologRuntimePlugin.getDefault()
				.getPrologInterfaceRegistry();
		String pifKey = pifRegistry.getKey(pif);
		Set<Subscription> subscriptions = pifRegistry
				.getSubscriptionsForPif(pifKey);

		if (ownSubscriptionMissing(subscriptions)) {
			FocusViewSubscription mySubscription = FocusViewSubscription
					.newInstance(pifKey);
			pif = PrologRuntimeUIPlugin.getDefault().getPrologInterface(
					mySubscription);
		}
		return pif;
	}

	private boolean ownSubscriptionMissing(Set<Subscription> subscriptions) {
		for (Subscription subscription : subscriptions) {
			if (subscription instanceof FocusViewSubscription)
				return false;
		}
		return true;
	}
}
