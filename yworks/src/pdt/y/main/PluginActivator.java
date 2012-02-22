package pdt.y.main;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import pdt.y.model.labels.NodeLabelConfigurationsInitializer;

/**
 * The activator class controls the plug-in life cycle
 */
public class PluginActivator extends AbstractUIPlugin {
	
	// The plug-in ID
	public static final String PLUGIN_ID = "pdt.yworks";
	
	private List<PreferencesUpdateListener> listeners = new LinkedList<PreferencesUpdateListener>();

	// The shared instance
	private static PluginActivator plugin;
	
	public PluginActivator() {
		NodeLabelConfigurationsInitializer.initialize();
	}

	
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	public static PluginActivator getDefault() {
		return plugin;
	}
	
	public void addPreferencesUpdateListener(PreferencesUpdateListener listener) {
		listeners.add(listener);
	}
	
	public void removePreferencesUpdateListener(PreferencesUpdateListener listener) {
		listeners.remove(listener);
	}
	
	public void preferencesUpdated() {
		for (PreferencesUpdateListener l : listeners) {
			l.preferencesUpdated();
		}
	}

	/**
	 * Returns an image descriptor for the image file at the given
	 * plug-in relative path
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}
}
