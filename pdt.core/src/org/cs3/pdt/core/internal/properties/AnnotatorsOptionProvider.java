package org.cs3.pdt.core.internal.properties;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.OptionProvider;
import org.cs3.pl.common.OptionProviderEvent;
import org.cs3.pl.common.OptionProviderExtension;
import org.cs3.pl.common.OptionProviderListener;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.DefaultAsyncPrologSessionListener;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;

public class AnnotatorsOptionProvider implements OptionProvider,OptionProviderExtension,PrologInterfaceListener {
	public static final String SUBJECT = "annotator_enabled";

	IPrologProject prologProject = null;

	private Option[] options;

	//private Map settings=new HashMap();
	private Vector listeners = new Vector();

	private boolean itsMe;
	public void addOptionProviderListener(OptionProviderListener l) {
		synchronized (listeners) {
			if(!listeners.contains(l)){
				listeners.add(l);
			}	
		}
	}

	public void removeOptionProviderListener(OptionProviderListener l) {
		if(listeners.contains(l)){
			listeners.remove(l);
		}
	}

	public synchronized void setPreferenceValues(String[] ids, String[] values) {
		for (int i = 0; i < values.length; i++) {
			String value= values[i];
			String id = ids[i];
			prologProject.setPreferenceValue("enabled."+id, value);
		}
		
		updatePrologBackend(ids,values);
		fireValuesChanged(ids);
	}

	private void updatePrologBackend(String[] ids, String[] values) {
		itsMe=true;
		try {
			AsyncPrologSession s = ((PrologInterface)prologProject.getMetadataPrologInterface()).getAsyncSession(PrologInterface.NONE);
			s.addBatchListener(new DefaultAsyncPrologSessionListener());
			for (int i = 0; i < values.length; i++) {
				s.queryOnce("setup annotators", "pdt_set_annotator_enabled("+ids[i]+", "+values[i]+")");
			}
			s.join();
			s.dispose();
		} catch (PrologInterfaceException e) {
			Debug.report(e);			
		}
		itsMe=false;
	}

	private void fireValuesChanged(String[] ids) {
		OptionProviderEvent e = new OptionProviderEvent(this,ids);
		Vector clone = new Vector();
		synchronized (listeners) {
			clone.addAll(listeners);
		}
		for (Iterator it = clone.iterator(); it.hasNext();) {
			OptionProviderListener l = (OptionProviderListener) it.next();
			l.valuesChanged(e);
		}
		
	}

	public Option[] getOptions() {
		return options;
	}

	public  AnnotatorsOptionProvider(IPrologProject plProject)  throws PrologInterfaceException {
		this.prologProject=plProject;
		PrologInterface pif = prologProject.getMetadataPrologInterface();
		
		
		PrologSession session = pif.getSession(PrologInterface.NONE);
		List l = session.queryAll("pdt_annotator_enabled(Annotator,Enabled)");
		options = new Option[l.size()];
		
		int i=0;
		Vector ids = new Vector();
		Vector values = new Vector();
		for (Iterator iter = l.iterator(); iter.hasNext();) {
			Map map = (Map) iter.next();
			String annotator = (String) map.get("Annotator");
			String enabled = (String) map.get("Enabled");
			options[i++] = new SimpleOption(annotator,annotator,"no description",Option.FLAG,"true");
			
			String persisted = prologProject.getPreferenceValue("enabled."+annotator, enabled);
			if(!persisted.equals(enabled)){
				ids.add(annotator);
				values.add(persisted);
			}
			
		}
		updatePrologBackend(
				(String[])ids.toArray(new String[ids.size()]), 
				(String[])values.toArray(new String[values.size()]));
		
	}

	public synchronized String getPreferenceValue(String id, String string) {		
		return prologProject.getPreferenceValue("enabled."+id, getDefault(id));
	}

	private String getDefault(String id) {
		for (int i = 0; i < options.length; i++) {
			Option option = options[i];
			if(option.getId().equals(id)){
				return option.getDefault();
			}
		}
		return null;
	}

	public void reconfigure() {


	}

	public void setPreferenceValue(String id, String value) {
		setPreferenceValues(new String[]{id}, new String[]{value});	
	}

	public synchronized void update(PrologInterfaceEvent e) {
		if(itsMe){
			return;
		}
		if(!SUBJECT.equals(e.getSubject())){
			return;
		}
		
		String[] eventdata=e.getEvent().split("-");
		setPreferenceValue(eventdata[0], eventdata[1]);
		
		
	}
	
	
}
