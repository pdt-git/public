package org.cs3.pdt.views;

import java.io.IOException;

import org.cs3.pdt.IPreferences;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PreferenceListener;
import org.cs3.pdt.PreferencesEvent;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Properties;
import org.cs3.pl.common.Util;
import org.cs3.pl.console.ConsoleView;
import org.cs3.pl.console.DefaultConsoleController;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;


public class PrologConsoleView extends ViewPart implements PreferenceListener,
		LifeCycleHook {

	private ConsoleView view;

	private PrologSocketConsoleModel model;

	private DefaultConsoleController controller;

	private PrologCompletionProvider completionProvider;

	public static final String HOOK_ID = "org.cs3.pdt.views.PrologConsoleView";

	public PrologConsoleView() {
	}

	public void createPartControl(Composite parent) {
		PDTPlugin plugin = PDTPlugin.getDefault();
		PrologInterface pi = null;
		try {
			pi = plugin.getPrologInterface();
		} catch (IOException e) {
			Debug.report(e);
		}
		pi.addLifeCycleHook(this, HOOK_ID,
				new String[] { CreateServerThreadHook.HOOK_ID });
		IPreferences preferences = plugin.getPreferences();
		preferences.addPreferencesListener(this);
		int port = Integer.parseInt(preferences.get(Properties.CONSOLE_PORT,
				"4711"));

		view = new ConsoleView();
		controller = new DefaultConsoleController();
		completionProvider = new PrologCompletionProvider();
		completionProvider.setPrologInterface(pi);
		controller.setCompletionProvider(completionProvider);
		view.setController(controller);

		model = new PrologSocketConsoleModel(false);
		model.setPort(port);
		view.createPartControl(parent);
		if (Util.probePort(port, "end_of_file.\n")) {
			model.connect();
		}
		//else: wait til the hook callback is called.
	}

	public void setFocus() {
		if (view == null) {
			Debug
					.warning("PrologConsoleView.setFocus(): View not instantiated yet.");
			return;
		}
		view.setFocus();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see prg.cs3.pdt.PreferenceListener#preferencesChanged(prg.cs3.pdt.PreferencesEvent)
	 */
	public void preferencesChanged(PreferencesEvent e) {
		if (e.getKeys().contains(Properties.CONSOLE_PORT)) {
			PDTPlugin plugin = PDTPlugin.getDefault();
			IPreferences preferences = plugin.getPreferences();
			int port = Integer.parseInt(preferences.get(
					Properties.CONSOLE_PORT, "4711"));
			model.disconnect();
			model.setPort(port);
			model.connect();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#onInit(org.cs3.pl.prolog.PrologSession)
	 */
	public void onInit(PrologSession initSession) {		
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	public void afterInit() {
		model.connect();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
	 */
	public void beforeShutdown(PrologSession session) {
		model.disconnect();
	}
}