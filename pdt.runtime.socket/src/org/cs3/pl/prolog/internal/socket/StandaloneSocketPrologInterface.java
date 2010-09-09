package org.cs3.pl.prolog.internal.socket;

import org.cs3.pl.common.PreferenceProvider;
import org.cs3.pl.common.Util;


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
		setCreateLogs(false);
		setUseSessionPooling("true");
		setServerLogDir(System.getProperty("java.io.tmpdir"));		
	}
}
