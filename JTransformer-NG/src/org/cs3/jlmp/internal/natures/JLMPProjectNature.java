package org.cs3.jlmp.internal.natures;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPPlugin;
import org.cs3.jlmp.JLMPProject;
import org.cs3.jlmp.JLMPProjectEvent;
import org.cs3.jlmp.JLMPProjectListener;
import org.cs3.jlmp.internal.builders.FactBaseBuilder;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.ICompilationUnit;

/**
 * @see IProjectNature
 */
public class JLMPProjectNature implements IProjectNature, JLMPProject, JLMPProjectListener {

    //The project for which this nature was requested,
    //see IProject.getNature(String)
    private IProject project;

    private FactBaseBuilder builder;

    private Vector listeners = new Vector();

    /**
     *  
     */
    public JLMPProjectNature() {
    }

    /**
     * @see IProjectNature#configure
     */
    public void configure() throws CoreException {
        Debug.debug("configure was called");
        IProjectDescription descr = project.getDescription();
        ICommand sheepBuilder = descr.newCommand();
        sheepBuilder.setBuilderName(JLMP.BUILDER_ID);
        ICommand builders[] = descr.getBuildSpec();
        for (int i = 0; i < builders.length; i++) {
            if (builders[i].getBuilderName().equals(JLMP.BUILDER_ID)) {
                return;
            }
        }
        ICommand newBuilders[] = new ICommand[builders.length + 1];
        System.arraycopy(builders, 0, newBuilders, 0, builders.length);
        newBuilders[builders.length] = sheepBuilder;
        descr.setBuildSpec(newBuilders);
        project.setDescription(descr, null);

        //important: touch the ConsultServices NOW if the pif is already
        // running.
        //otherwise recorded facts might be reloaded to late! (i.e. by the
        // builder AFTER it has
        //"found" unresolved types
        PrologInterface pif = getPrologInterface();
        if (pif.isUp()) {
            pif.getConsultService(JLMP.EXT).setRecording(true);
            pif.getConsultService(JLMP.SRC).setRecording(true);
            pif.getConsultService(JLMP.EXT).setAppendingRecords(true);
        }
    }

    /**
     * @see IProjectNature#deconfigure
     */
    public void deconfigure() throws CoreException {
        IProjectDescription descr = project.getProject().getDescription();
        Debug.debug("deconfigure was called");
        ICommand builders[] = descr.getBuildSpec();
        int index = -1;
        for (int i = 0; i < builders.length; i++) {
            if (builders[i].getBuilderName().equals(JLMP.BUILDER_ID)) {
                index = i;
                break;
            }
        }
        if (index != -1) {
            ICommand newBuilders[] = new ICommand[builders.length - 1];
            System.arraycopy(builders, 0, newBuilders, 0, index);
            System.arraycopy(builders, index + 1, newBuilders, index,
                    builders.length - index - 1);
            descr.setBuildSpec(newBuilders);
        }
    }

    /**
     * @see IProjectNature#getProject
     */
    public IProject getProject() {
        return project;
    }

    /**
     * @see IProjectNature#setProject
     */
    public void setProject(IProject project) {
        this.project = project;
    }

    public PrologInterface getPrologInterface() {
        PrologInterface pif = PDTPlugin.getDefault().getPrologInterface(
                getProject().getName());

        //	    List libraries = pif.getBootstrapLibraries();
        //	    ResourceFileLocator l =
        // JLMPPlugin.getDefault().getResourceLocator(JLMP.ENGINE);
        //	    String name = Util.prologFileName(l.resolve(JLMP.MAIN_PL));
        //	    if(!libraries.contains(name)){
        //	        libraries.add(name);
        //	        if(pif.isUp());
        //	        PrologSession s = pif.getSession();
        //	        s.queryOnce("['"+name+"']");
        //	    }
        return pif;
    }

    /**
     * @return Returns the builder.
     */
    public FactBaseBuilder getFactBaseBuilder() {
        if (builder == null) {
            builder = new FactBaseBuilder(getProject(), getPrologInterface());
            builder.addJLMPProjectListener(this);
        }
        return builder;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jlmp.JLMPProject#addJLMPProjectListener(org.cs3.jlmp.JLMPProjectListener)
     */
    public void addJLMPProjectListener(JLMPProjectListener l) {
        synchronized (listeners) {
            if (!listeners.contains(l)) {
                listeners.add(l);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jlmp.JLMPProject#removeJLMPProjectListener(org.cs3.jlmp.JLMPProjectListener)
     */
    public void removeJLMPProjectListener(JLMPProjectListener l) {
        synchronized (listeners) {
            if (listeners.contains(l)) {
                listeners.remove(l);
            }
        }
    }

    protected void fireFactBaseUpdated(){
        Runnable r = new Runnable() {
            public void run() {
                JLMPProjectEvent e = new JLMPProjectEvent(this);
                
                JLMPPlugin.getDefault().fireFactBaseUpdated(e);
                
                Vector cloned = null;
                synchronized (listeners) {
                    cloned = (Vector) listeners.clone();
                }
                for (Iterator it = cloned.iterator(); it.hasNext();) {
                    JLMPProjectListener l = (JLMPProjectListener) it.next();
                    l.factBaseUpdated(e);
                }
                
            }
        };
        Thread t = new Thread(r,"JLMPProject listener notification");
        t.setDaemon(true);
        t.start();
    }
    /* (non-Javadoc)
     * @see org.cs3.jlmp.JLMPProject#generateFacts(org.eclipse.jdt.core.ICompilationUnit, java.io.PrintStream)
     */
    public void generateFacts(ICompilationUnit cu, PrintStream out) throws IOException, CoreException {
       getFactBaseBuilder().writeFacts(cu,out);
        
    }

    /* (non-Javadoc)
     * @see org.cs3.jlmp.JLMPProjectListener#factBaseUpdated(org.cs3.jlmp.JLMPProjectEvent)
     */
    public void factBaseUpdated(JLMPProjectEvent e) {
      fireFactBaseUpdated();  
    }

}