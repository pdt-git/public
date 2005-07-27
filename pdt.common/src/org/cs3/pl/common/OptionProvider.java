package org.cs3.pl.common;


public interface OptionProvider {

	public Option[] getOptions();
	public void reconfigure();

	public String getPreferenceValue(String id, String string);
	public void setPreferenceValue(String id, String value);

}
