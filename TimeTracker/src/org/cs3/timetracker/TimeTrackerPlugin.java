package org.cs3.timetracker;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import sun.misc.Resource;

/**
 * The main plugin class to be used in the desktop.
 */
public class TimeTrackerPlugin extends AbstractUIPlugin {
	//The shared instance.
	private static TimeTrackerPlugin plugin;
	//Resource bundle.
	private ResourceBundle resourceBundle;
	private boolean isCountingUp = true;
	
	/**
	 * The constructor.
	 */
	public TimeTrackerPlugin() {
		super();
		plugin = this;
	
		try {
			resourceBundle = ResourceBundle.getBundle("TimeTracker.TimeTrackerPluginResources");
		} catch (MissingResourceException x) {
			resourceBundle = null;
		}
		ResourcesP
	}

	/**
	 * This method is called upon plug-in activation
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		updatePreferences();
	}

	/**
	 * This method is called when the plug-in is stopped
	 */
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		savePluginPreferences();
	}

	/**
	 * Returns the shared instance.
	 */
	public static TimeTrackerPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the string from the plugin's resource bundle,
	 * or 'key' if not found.
	 */
	public static String getResourceString(String key) {
		ResourceBundle bundle = TimeTrackerPlugin.getDefault().getResourceBundle();
		try {
			return (bundle != null) ? bundle.getString(key) : key;
		} catch (MissingResourceException e) {
			return key;
		}
	}

	/**
	 * Returns the plugin's resource bundle,
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}
	
	public boolean isCountingUp(){
		return isCountingUp;
	}
	
	/*
	 * Initialize the preference page.
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#initializeDefaultPreferences(org.eclipse.jface.preference.IPreferenceStore)
	 */
	
    protected void initializeDefaultPreferences(IPreferenceStore store) {
		store.setDefault(TimeTrackerPreferencePage.P_IS_COUNTING_UP, true);
	}
    
	public void updatePreferences() { 
		try { 
		IPreferenceStore store = getPreferenceStore();
		isCountingUp = store.getBoolean(TimeTrackerPreferencePage.P_IS_COUNTING_UP);
		} catch(Exception e) {
			e.printStackTrace();
		}
		// update all the variables based on the new values in the preference store
		
	}

}
