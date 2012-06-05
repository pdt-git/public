package org.cs3.prolog.common;

public interface OptionProviderExtension {
	public void addOptionProviderListener(OptionProviderListener l);
	public void removeOptionProviderListener(OptionProviderListener l);
	public void setPreferenceValues(String[] ids,String[] values);
}
