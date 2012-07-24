/* $LICENSE_MSG$(ld) */

package org.cs3.pdt;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.cs3.pdt.internal.editors.ColorManager;
import org.cs3.pdt.internal.editors.CurrentPifListener;
import org.cs3.pdt.internal.editors.EditorConsultListener;
import org.cs3.prolog.common.OptionProviderListener;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.ui.util.DefaultErrorMessageProvider;
import org.cs3.prolog.ui.util.ErrorMessageProvider;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class PDTPlugin extends AbstractUIPlugin implements IStartup, ISelectionProvider{
	
	private List<ISelectionChangedListener> changeListeners = new ArrayList<ISelectionChangedListener>();
	private ISelection selection;

	public static final String MODULEPREFIX = "pdtplugin:";

	private static ColorManager colorManager; 
	
	// The shared instance.
	private static PDTPlugin plugin;

	/**
	 * Returns the shared instance.
	 */
	public static PDTPlugin getDefault() {
		return plugin;
	}

	private DefaultErrorMessageProvider errorMessageProvider;

	/**
	 * The constructor.
	 */
	public PDTPlugin() {
		super();
		plugin = this;
	}

	/**
	 * look up a preference value.
	 * <p>
	 * will return user settings if available or default settings if not. If a
	 * system property with the given key is defined it will overrule any
	 * existing setting in the preference store. if the key is not defined, this
	 * method returns the given default..
	 * 
	 * @param key
	 * @return the value or specified default if no such key exists..
	 */
	public String getPreferenceValue(String key, String defaultValue) {

		IPreferencesService service = Platform.getPreferencesService();
		String qualifier = getBundle().getSymbolicName();
		String value = service.getString(qualifier, key, defaultValue, null);
		return System.getProperty(key, value);
	}

	public void setPreferenceValue(String key, String value) {
		getPreferenceStore().setValue(key, value);
	}

	/**
	 * 
	 */
	public void reconfigure() {
		try {
			reconfigureDebugOutput();

		} catch (Throwable e) {
			Debug.report(e);
		}
	}

	private void reconfigureDebugOutput() throws FileNotFoundException {
		String debugLevel = getPreferenceValue(PDT.PREF_DEBUG_LEVEL, "WARNING");
		String debugOutputTo = getPreferenceValue(PDT.PREF_DEBUG_OUTPUT_TO, "LOGFILE");
		String logFileName = getPreferenceValue(PDT.PREF_CLIENT_LOG_FILE_DIR, System.getProperty("java.io.tmpdir"));
		
		Debug.setDebugLevel(debugLevel);
		Debug.setLogDir(logFileName);	
		Debug.setOutputTo(debugOutputTo);
		
		
	}

	/**
	 * This method is called upon plug-in activation
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		try {
			super.start(context);
			reconfigureDebugOutput();
			IPropertyChangeListener debugPropertyChangeListener = new IPropertyChangeListener() {
				@Override
				public void propertyChange(PropertyChangeEvent e) {
					try {
						PDTPlugin.getDefault().reconfigureDebugOutput();
					} catch (FileNotFoundException e1) {
						Debug.report(e1);
					}
				}

			};	
			getPreferenceStore().addPropertyChangeListener(debugPropertyChangeListener);
			
			PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().registerActivePrologInterfaceListener(new CurrentPifListener());
			PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().registerConsultListener(new EditorConsultListener());
		} catch (Throwable t) {
			Debug.report(t);
		}
	}

	public String getId() {
		return getBundle().getSymbolicName();
	}

	public ErrorMessageProvider getErrorMessageProvider() {
		if (errorMessageProvider == null) {
			errorMessageProvider = new DefaultErrorMessageProvider(this);
		};
		return errorMessageProvider;
	}

	public static IWorkbenchPage getActivePage() {
		final IWorkbenchWindow activeWorkbenchWindow = PlatformUI
			.getWorkbench().getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null) {
				return null;
		}
		return  activeWorkbenchWindow.getActivePage();
	}
	

	@Override
	public void earlyStartup() {
	}

	/**
	 * Returns a section in the Prolog plugin's dialog settings. If the section doesn't exist yet, it is created.
	 *
	 * @param name the name of the section
	 * @return the section of the given name
	 */
	public IDialogSettings getDialogSettingsSection(String name) {
		IDialogSettings dialogSettings= getDialogSettings();
		IDialogSettings section= dialogSettings.getSection(name);
		if (section == null) {
			section= dialogSettings.addNewSection(name);
		}
		return section;
	}

	public ColorManager getColorManager() {
		if(colorManager == null) {
			colorManager = new ColorManager();
		}
		return colorManager;
	}


	@Override
	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		synchronized (changeListeners) {
			changeListeners.add(listener);
		}
	}

	@Override
	public void removeSelectionChangedListener(
			ISelectionChangedListener listener) {
		synchronized (changeListeners) {
			changeListeners.remove(listener);
		}
	}
	
	@Override
	public void setSelection(ISelection selection) {
		this.selection = selection;
		informListenersAboutEditorContent(selection);
	}
	
	public void informListenersAboutEditorContent(ISelection selection) {
		synchronized (changeListeners) {
			for (ISelectionChangedListener listener : changeListeners) {
				listener.selectionChanged(new SelectionChangedEvent(this, selection));
			}
		}
	}

	@Override
	public ISelection getSelection() {
		return selection;
	}

	
	
	Set<OptionProviderListener> decorators = new HashSet<OptionProviderListener>();
	
	public void addDecorator(OptionProviderListener decorator) {
		decorators.add(decorator);
	}
	
	public void removeDecorator(OptionProviderListener decorator) {
		decorators.remove(decorator);
	}
	
	public void notifyDecorators() {
		for (OptionProviderListener d : decorators) {
			d.valuesChanged(null);
		}
	}
	
}

