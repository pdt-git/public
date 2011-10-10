package org.cs3.pl.jpl;

import java.util.HashMap;
import java.util.Map;

public class JPLHooks {
	private static Map<String, JPLHook> jplHooks = new HashMap<String, JPLHook>();

	static public void putJPLHook(String name, JPLHook hook) {
		jplHooks.put(name,hook);
	}


	public static Object jplHook(String hookName, Object[] args){
		if(jplHooks.containsKey(hookName)){
			return jplHooks.get(hookName).hook(hookName, args);
		}
		return null;
	}
}
