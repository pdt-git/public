package org.cs3.pl.common;

public interface OptionProviderExtension {
	public void addOptionProviderListener(OptionProviderListener l);
	public void removeOptionProviderListener(OptionProviderListener l);
	public void setPreferenceValues(String[] ids,String[] values);
}
