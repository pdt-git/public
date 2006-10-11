package org.cs3.pl.tuprolog.internal;

import java.io.IOException;

import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.ServerStartAndStopStrategy;

import alice.tuprolog.InvalidTheoryException;
import alice.tuprolog.Prolog;

public class TuPrologServerStartAndStopStrategy implements ServerStartAndStopStrategy {

	private TuProlog engine;

	public TuPrologServerStartAndStopStrategy(TuProlog engine) {
		this.engine = engine;
	}
	
	public boolean isRunning(PrologInterface pif) {
		return !engine.isHalted();
	}

	public Process startServer(PrologInterface pif) {
		engine.clearTheory();
		try {
			engine.initEngine();
		} catch (InvalidTheoryException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null; // TODO: TRHO: @Lukas: why is a Process returned here? 
	}

	public void stopServer(PrologInterface pif) {
		engine.clearTheory();
	}

}
