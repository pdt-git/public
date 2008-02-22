package org.cs3.pdt.runtime.internal;

import java.io.File;

import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.LifeCycleHook2;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;

public class LifeCycleHookDecorator implements LifeCycleHook2{
	private PrologLibraryManager libmgr;
	private String[] dependencies;
	private File[] consults;
	private LifeCycleHook target;
	private LifeCycleHookDescriptor descriptor;
	private Object data;
	
	@Override
	public boolean equals(Object obj) {
		if(!(obj instanceof LifeCycleHookDecorator)){
			return false;
		}
		LifeCycleHookDecorator other = (LifeCycleHookDecorator) obj;
		return this.descriptor.equals(other.descriptor)&&this.data.equals(other.data);
		
	}
	

	public LifeCycleHookDecorator(LifeCycleHook hook,
			String[] libraryDependencies, File[] consults,
			PrologLibraryManager libraryManager) {
		this.target=hook;
		this.dependencies=libraryDependencies;
		this.libmgr=libraryManager;
		this.consults=consults;
	}

	@Override
	public void onError(PrologInterface pif) {
		if(target!=null&& (target instanceof LifeCycleHook2))
		((LifeCycleHook2)target).onError(pif);
		
	}

	@Override
	public void setData(Object data) {
		if(target!=null&& (target instanceof LifeCycleHook2)){
			((LifeCycleHook2)target).setData(data);
		}
		
	}

	@Override
	public void afterInit(PrologInterface pif) throws PrologInterfaceException {
		if(target!=null){
			target.afterInit(pif);
		}
		
	}

	@Override
	public void beforeShutdown(PrologInterface pif, PrologSession session)
			throws PrologInterfaceException {
		if(target!=null){
			target.beforeShutdown(pif, session);
		}
		
	}

	@Override
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

}
