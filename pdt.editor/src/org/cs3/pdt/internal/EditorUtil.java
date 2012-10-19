package org.cs3.pdt.internal;

import java.util.List;

public class EditorUtil {
	
	public static String getProperty(String property, List<String> properties) {
		for (String p : properties) {
			if (p.startsWith(property)) {
				return p.substring(property.length() + 1, p.length() - 1);
			}
		}
		return null;
	}
	
}
