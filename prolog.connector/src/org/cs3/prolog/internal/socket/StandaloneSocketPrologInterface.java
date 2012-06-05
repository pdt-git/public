package org.cs3.prolog.internal.socket;

import org.cs3.prolog.common.PreferenceProvider;
import org.cs3.prolog.common.Util;


public class StandaloneSocketPrologInterface extends SocketPrologInterface {

	public StandaloneSocketPrologInterface(String name) {	
		super(name);
	}
	
	@Override
	public void initOptions(PreferenceProvider p ){
		setStandAloneServer("false");
		setHost("localhost");
		setExecutable(Util.guessExecutableName());
		setEnvironment(Util.guessEnvironmentVariables());
		setTimeout("15000");
		setServerPort("9944");
		setHidePlwin(true);
		setUseSessionPooling(true);
	}
}
