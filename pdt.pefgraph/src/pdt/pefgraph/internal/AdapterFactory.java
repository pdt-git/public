package pdt.pefgraph.internal;

import java.util.Map;

import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.core.PEFHandle;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.views.properties.IPropertySource;

public class AdapterFactory implements IAdapterFactory {

	public Object getAdapter(Object o, Class t) {
		if (o == null) {
			return null;
		}
		try {
			if (PEFHandle.class.equals(t)) {
				return getHandle(o);
			}
			else if(IPropertySource.class.equals(t)){
				return getPropertySource(o);
			}
		} catch (CoreException e) {
			Debug.rethrow(e);
		}
		return null;
	}

	private Object getPropertySource(Object o) throws CoreException {
		if(o instanceof PEFHandle){
			return createPropertySource((PEFHandle) o);
		}
		else{
			PEFHandle handle = (PEFHandle) getHandle(o);
			if(handle==null){
				return null;
			}
			else{
				return createPropertySource(handle);
			}
		}
		
	}

	private Object getHandle(Object o) throws CoreException {
		if (o instanceof IMarker) {
			IMarker m = (IMarker) o;

			if (PDTCore.PROBLEM.equals(m.getType())) {
				return createHandle(m);
			}

		} else if (o instanceof IResource) {
			IResource r = (IResource) o;
			return createHandle(r);
		} else if (o instanceof PEFHandle) {
			return createPropertySource((PEFHandle) o);
		}

		return null;
	}

	private Object createPropertySource(PEFHandle handle) {

		return new PEFPropertySource(handle);

	}

	private Object createHandle(IResource r) throws CoreException {
		final PrologInterface pif = PDTCoreUtils.getPrologProject(r)
				.getMetadataPrologInterface();
		int type = r.getType();
		IProject project = r.getProject();
		String path = Util.prologFileName(r.getLocation().toFile());
		String query = null;
		PrologSession s = null;
		switch (type) {
		case IResource.PROJECT:
			query = "pef_project_query([name='" + r.getName() + "', id=ID])";
			break;
		case IResource.FOLDER:

			query = "pef_directory_query([path='" + path
					+ "', include_pattern=IP,exclude_pattern=EP,id=ID),"
					+ "pdt_belongs_to_star(directory('" + path
					+ "',IP,EP),project('" + project.getName() + "'))";
			break;
		case IResource.FILE:
			query = "get_pef_file('" + path + "',ID)";
			break;

		default:
			query = "fail";
		}
		try {
			s = pif.getSession();
			Map m = s.queryOnce(query);
			if (m == null) {
				return null;
			}
			final String id = (String) m.get("ID");
			return new PEFHandle() {

				public String getId() {
					return id;
				}

				public PrologInterface getPrologInterface() {
					return pif;
				}

				public Object getAdapter(Class adapter) {
					return Platform.getAdapterManager().getAdapter(this,
							adapter);
				}

			};

		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
		return null;
	}

	private Object createHandle(IMarker m) throws CoreException {
		final String id = (String) m.getAttribute(PDTCore.PROBLEM_ID);
		final PrologInterface pif = PDTCoreUtils.getPrologProject(
				m.getResource()).getMetadataPrologInterface();
		return new PEFHandle() {

			public String getId() {
				return id;
			}

			public PrologInterface getPrologInterface() {
				return pif;
			}

			public Object getAdapter(Class adapter) {
				return Platform.getAdapterManager().getAdapter(this, adapter);
			}
		};
	}

	public Class[] getAdapterList() {
		return new Class[] { PEFHandle.class, IPropertySource.class };
	}

}
