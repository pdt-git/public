package org.cs3.pdt.views;


import java.io.IOException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Properties;
import org.cs3.pl.console.ConsoleView;
import org.cs3.pl.console.DefaultConsoleController;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;

import prg.cs3.pdt.IPreferences;
import prg.cs3.pdt.PDTPlugin;
import prg.cs3.pdt.PreferenceListener;
import prg.cs3.pdt.PreferencesEvent;


public class PrologConsoleView extends ViewPart implements PreferenceListener {	
	
	private ConsoleView view;
	private PrologSocketConsoleModel model;
	private DefaultConsoleController controller;
	private PrologCompletionProvider completionProvider;

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
		
		IPreferences preferences = plugin.getPreferences();
		preferences.addPreferencesListener(this);
		int port = Integer.parseInt(preferences.get(Properties.CONSOLE_PORT,"4711"));		
		
		view = new ConsoleView();
		controller = new DefaultConsoleController();
		completionProvider = new PrologCompletionProvider();
		completionProvider.setPrologInterface(pi);
		controller.setCompletionProvider(completionProvider);
		view.setController(controller);
		
		model=new PrologSocketConsoleModel(false);
		model.setPort(port);
        view.createPartControl(parent);
        
	}

	public void setFocus() {
		if(view==null){
			Debug.warning("PrologConsoleView.setFocus(): View not instantiated yet.");
			return;
		}
		view.setFocus();
	}

	/* (non-Javadoc)
	 * @see prg.cs3.pdt.PreferenceListener#preferencesChanged(prg.cs3.pdt.PreferencesEvent)
	 */
	public void preferencesChanged(PreferencesEvent e) {
		if(e.getKeys().contains(Properties.CONSOLE_PORT)){
			PDTPlugin plugin = PDTPlugin.getDefault();
			IPreferences preferences = plugin.getPreferences();
			int port = Integer.parseInt(preferences.get(Properties.CONSOLE_PORT,"4711"));
			model.disconnect();
			model.setPort(port);
			model.connect();
		}
	}
}