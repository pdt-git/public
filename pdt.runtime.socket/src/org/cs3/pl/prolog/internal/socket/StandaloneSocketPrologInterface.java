package org.cs3.pl.prolog.internal.socket;

import org.cs3.pdt.runtime.preferences.PreferenceInitializer;

public class StandaloneSocketPrologInterface extends SocketPrologInterface {

	public StandaloneSocketPrologInterface(String name) {	
		super(name);
	}
	
	@Override
	public void initOptions(){
		setStandAloneServer("false");
		setHost("localhost");
		setExecutable(PreferenceInitializer.guessExecutableName());
		setEnvironment(PreferenceInitializer.guessEnvironmentVariables());
		setTimeout("15000");
		setServerPort("9944");
		setHidePlwin(true);
		setCreateLogs(false);
		setUseSessionPooling("true");
		setServerLogDir(System.getProperty("java.io.tmpdir"));		
	}
}
