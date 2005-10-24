package org.cs3.pl.common;


public interface OptionProvider {

	public Option[] getOptions();
	public void reconfigure();

	
	/* maybe the following two should be in some other interface?
	 * pif factories don't support them (now), still they provide options
	 * 
	 * preference pages cannot quiet use them... afaics
	 */ 
	public String getPreferenceValue(String id, String string);
	public void setPreferenceValue(String id, String value);

}
