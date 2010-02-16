package org.cs3.pdt.runtime.internal;

import java.io.File;

import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;

public class LifeCycleHookProxy implements LifeCycleHook{
	private PrologLibraryManager libmgr;
	private String[] dependencies;
	private File[] consults;
	private LifeCycleHook target;
	private LifeCycleHookDescriptor descriptor;
	private Object data;
	
	public LifeCycleHookProxy(LifeCycleHook hook,
			String[] libraryDependencies, File[] consults,
			PrologLibraryManager libraryManager) {
		this.target=hook;
		this.dependencies=libraryDependencies;
		this.libmgr=libraryManager;
		this.consults=consults;
	}

	public boolean equals(Object obj) {
		if(!(obj instanceof LifeCycleHookProxy)){
			return false;
		}
		LifeCycleHookProxy other = (LifeCycleHookProxy) obj;
		return this.descriptor.equals(other.descriptor)&&this.data.equals(other.data);
	}
	
	public void onError(PrologInterface pif) {
		if(target!=null)
			target.onError(pif);
	}

	public void setData(Object data) {
		if(target!=null){
			target.setData(data);
		}
	}

	public void afterInit(PrologInterface pif) throws PrologInterfaceException {
		if(target!=null){
			target.afterInit(pif);
		}	
	}

	public void beforeShutdown(PrologInterface pif, PrologSession session)
			throws PrologInterfaceException {
		if(target!=null){
			target.beforeShutdown(pif, session);
		}
	}
	
	public void onInit(PrologInterface pif, PrologSession initSession)
			throws PrologInterfaceException {
		PLUtil.configureFileSearchPath(libmgr, initSession, dependencies);
		for (int i = 0; i < consults.length; i++) {
			initSession.queryOnce("ensure_loaded('"+Util.prologFileName(consults[i])+"')");
		}
		if(target!=null){
			target.onInit(pif, initSession);
		}
	}
	
	public void lateInit(PrologInterface pif) {
		;
	}
}
