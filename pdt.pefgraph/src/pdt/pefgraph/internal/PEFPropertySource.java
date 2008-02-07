package pdt.pefgraph.internal;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.core.PEFHandle;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.PropertyDescriptor;

public class PEFPropertySource implements IPropertySource {

	private IPropertyDescriptor[] descriptors;
	private Map<Object, Object> data;
	private PrologInterface pif;
	private String id;

	public PEFPropertySource(PEFHandle h) {
		this.pif=h.getPrologInterface();
		this.id=h.getId();
	}

	public Object getEditableValue() {

		return null;
	}

	public IPropertyDescriptor[] getPropertyDescriptors() {
		if(descriptors==null){
			createDescriptors();
		}
		return descriptors;
	}

	private void createDescriptors() {
		init();
		
	}

	private void fetchData() {
		init();
		
	}
	
	private void init() {
		if(pif==null||!pif.isUp()){
			return;
		}
		PrologSession s = null;
		data = new HashMap<Object, Object>();
		try {
			s=pif.getSession();
			List l = s.queryAll("pef_graph_property(Category,"+id+", Key,Value)");
			descriptors = new IPropertyDescriptor[l.size()];			
			int i=0;
			for (Object object : l) {
				Map m = (Map) object;
				String key = (String) m.get("Key");				
				PropertyDescriptor descriptor = new PropertyDescriptor(key,key);
				descriptor.setCategory((String) m.get("Category"));
				descriptors[i++]=descriptor;
				data.put(key, m.get("Value"));				
			}
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}finally{
			if(s!=null ){
				s.dispose();
			}
		}
		
	}

	public Object getPropertyValue(Object id) {
		if(data==null){
			fetchData();
		}
		return data.get(id);
	}

	

	public boolean isPropertySet(Object id) {
		// TODO Auto-generated method stub
		return false;
	}

	public void resetPropertyValue(Object id) {
		// TODO Auto-generated method stub
		
	}

	public void setPropertyValue(Object id, Object value) {
		// TODO Auto-generated method stub
		
	}

}
