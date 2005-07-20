/*
 */
package org.cs3.pdt.decorator.internal;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.cs3.pdt.IPrologProject;
import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.decorator.DecoratorPlugin;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.ui.internal.Workbench;

/**
 */
public class ConsultedDecorator implements ILightweightLabelDecorator {
    Vector listeners = new Vector();

    private HashMap consulted = new HashMap();

    private PrologInterfaceListener pifListener;

    private LifeCycleHook pifHook;

    private final static String CONSULTED_ICON = "consulted.gif";

    private final static String MODIFIED_ICON = "modified.gif";

   

    private IResourceChangeListener wsListener;

    private static final String LAST_CONSULTED = null;

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.ILightweightLabelDecorator#decorate(java.lang.Object,
     *         org.eclipse.jface.viewers.IDecoration)
     */
    public void decorate(Object obj, IDecoration decoration) {
        IFile file = null;
        if (obj instanceof IFile) {
            file = (IFile) obj;
        } else if (obj instanceof IAdaptable) {
            IAdaptable a = (IAdaptable) obj;
            IResource r = (IResource) a.getAdapter(IResource.class);
            if (r != null && IResource.FILE == r.getType()) {
                file = (IFile) r;
            }
        }
        if (file == null) {
            return;
        }
		Long consultStamp = (Long) consulted.get(getCanonicalPath(file));
        if (consultStamp == null) {
            return;
        }
		long modifiedStamp = file.getLocalTimeStamp();
		
		if (modifiedStamp > consultStamp.longValue()) {
            decoration.addOverlay(getImageDescriptor(MODIFIED_ICON));
        } else {
            decoration.addOverlay(getImageDescriptor(CONSULTED_ICON));
        }
    }

    public String getCanonicalPath(IResource r) {
        try {
            return new File(r.getLocation().toOSString()).getCanonicalPath();
        } catch (IOException e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }
    }

    public String getCanonicalPath(String s) {
        try {
            return new File(s).getCanonicalPath();
        } catch (IOException e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }
    }

    public ImageDescriptor getImageDescriptor(String icon) {
        URL url = DecoratorPlugin.getDefault().getBundle().getEntry("/icons/" + icon);
        ImageDescriptor desc = ImageDescriptor.createFromURL(url);
		return desc;
    }

    public ConsultedDecorator() {
        pifListener = new PrologInterfaceListener() {

            public void update(PrologInterfaceEvent e) {
                if (PrologInterface.SUBJECT_CONSULTED.equals(e.getSubject())) {
                    ConsultedDecorator.this.update((PrologInterface) e
                            .getSource(), e.getEvent());
                }
            }

        };

        pifHook = new LifeCycleHook() {

            public void onInit(PrologInterface pif, PrologSession initSession) {
                ;
            }

            public void afterInit(PrologInterface pif) {
                poll(pif);
            }

            public void beforeShutdown(PrologInterface pif,
                    PrologSession session) {
                clear();
            }

        };
		PDTPlugin r1 = PDTPlugin.getDefault();

        PrologInterface pif = PrologRuntimePlugin.getDefault().getPrologInterface();
        pif.addPrologInterfaceListener(PrologInterface.SUBJECT_CONSULTED,
                pifListener);
        pif.addLifeCycleHook(pifHook, "consulteddecorator", new String[0]);

        wsListener = new IResourceChangeListener() {

            public void resourceChanged(IResourceChangeEvent event) {
                IResourceDelta delta = event.getDelta();
                if (delta != null) {
                    try {
                        delta.accept(new IResourceDeltaVisitor() {
                            public boolean visit(IResourceDelta delta)
                                    throws CoreException {
                                IResource r = delta.getResource();
                                IProject project = r.getProject();
                                if (project == null) {
                                    //the workspace root
                                    return true;
                                }
                                if (!project.hasNature(PDT.NATURE_ID)) {
                                    //another prohect...
                                    return false;
                                }
                                if (r.getType() == IResource.PROJECT) {
                                    //a prolog Project
                                    return true;
                                }
                                IPrologProject plProject = (IPrologProject) project
                                        .getNature(PDT.NATURE_ID);
                                if (plProject.isPrologSource(r)) {
                                    if (r.getType() == IResource.FILE) {
                                        modified((IFile) r);
                                        return false;
                                    }
                                    return true;
                                }
                                return false;
                            }

                        });
                    } catch (CoreException e) {
                        Debug.report(e);
                        throw new RuntimeException(e);
                    }
                }
            }

        };
        ResourcesPlugin.getWorkspace().addResourceChangeListener(wsListener);
    }

    private void modified(IFile file) {
        fire(file);
    }

    private void fire(String filename){
        final Vector cloned;
        synchronized (listeners) {
            cloned = (Vector) listeners.clone();
        }
        
        IFile[] files = PDTUtils.findFilesForLocation(filename);
        final LabelProviderChangedEvent e = new LabelProviderChangedEvent(this,files);
        Workbench.getInstance().getDisplay().asyncExec(new Runnable() {
            public void run() {
                for (Iterator iter = cloned.iterator(); iter.hasNext();) {
                    ILabelProviderListener l = (ILabelProviderListener) iter
                            .next();
                    l.labelProviderChanged(e);
                }
            }
        });
    }
    
    /**
     * @param file
     */
    private void fire(IFile file) {
        final Vector cloned;
        synchronized (listeners) {
            cloned = (Vector) listeners.clone();
        }
        final LabelProviderChangedEvent e = new LabelProviderChangedEvent(this,file);
        Workbench.getInstance().getDisplay().asyncExec(new Runnable() {
            public void run() {
                for (Iterator iter = cloned.iterator(); iter.hasNext();) {
                    ILabelProviderListener l = (ILabelProviderListener) iter
                            .next();
                    l.labelProviderChanged(e);
                }
            }
        });

    }

    /**
     * @param pif
     * @param session
     */
    protected void clear() {
        Set s = new HashSet();
        s.addAll(consulted.keySet());
        consulted.clear();
        for (Iterator iter = s.iterator(); iter.hasNext();) {
            String key= (String) iter.next();

         
            fire(key);
        }
    }

    /**
     * @param pif
     */
    protected void poll(PrologInterface pif) {
        clear();
        PrologSession session = pif.getSession();
        try {
            List list = session.queryAll("'$time_source_file'(File,Time),prolog_to_os_filename(File,OSFile)");
            for (Iterator iter = list.iterator(); iter.hasNext();) {
                Map m = (Map) iter.next();
                String s = getCanonicalPath((String)m.get("OSFile"));
                
                try {
                    consulted.put(getCanonicalPath(s),new Long(Util.parsePrologTimeStamp((String) m.get("Time"))));
                } catch (NumberFormatException e) {
                    Debug.report(e);
                    throw new RuntimeException(e);
                }
                fire(s);
            }
        } finally {
            session.dispose();
        }
    }

    private void update(PrologInterface pif, String event) {
        PrologSession session = pif.getSession();
        Map m=null;
        try {

            m = session
                    .queryOnce("'$time_source_file'(File,Time),same_file(File,'"
                            + event + "')");
        } finally {
            session.dispose();
        }
            if (m == null) {
                return;
            }
            consulted.put(getCanonicalPath(event),new Long(Util.parsePrologTimeStamp((String) m.get("Time"))));
            
            
            fire(event);
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
     */
    public void addListener(ILabelProviderListener listener) {
        synchronized (listeners) {
            if (!listeners.contains(listener)) {
                listeners.add(listener);
            }
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
     */
    public void dispose() {
        PDTPlugin r = PDTPlugin.getDefault();
		PrologInterface pif = PrologRuntimePlugin.getDefault().getPrologInterface();
        pif.removePrologInterfaceListener(PrologInterface.SUBJECT_CONSULTED,
                pifListener);
        pif.removeLifeCycleHook("consulteddecorator");
        ResourcesPlugin.getWorkspace().removeResourceChangeListener(wsListener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object,
     *         java.lang.String)
     */
    public boolean isLabelProperty(Object element, String property) {
        // TODO Auto-generated method stub
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
     */
    public void removeListener(ILabelProviderListener listener) {
        synchronized (listeners) {
            if (listeners.contains(listener)) {
                listeners.remove(listener);
            }
        }

    }

}
