package org.cs3.pdt;

import java.util.Map;

public interface IPreferences {
	public void addPreferencesListener(PreferenceListener l);

	public void removePreferencesListener(PreferenceListener l);

	public String get(String key);

	public String get(String key, String dfltVal);

	public void set(String key, String value);

	public void set(Map keyToValue);

	
}