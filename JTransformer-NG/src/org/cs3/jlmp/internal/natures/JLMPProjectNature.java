package org.cs3.jlmp.internal.natures;

import java.io.File;
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
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.ICompilationUnit;

/**
 * @see IProjectNature
 */
public class JLMPProjectNature implements IProjectNature, JLMPProject,
        JLMPProjectListener {

    // The project for which this nature was requested,
    // see IProject.getNature(String)
    private IProject project;

    private Option[] options;

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
        getFactBaseBuilder().clean(null);
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

        // important: touch the ConsultServices NOW if the pif is already
        // running.
        // otherwise recorded facts might be reloaded to late! (i.e. by the
        // builder AFTER it has
        // "found" unresolved types
        PrologInterface pif = getPrologInterface();

        Job j = new Job("building PEFs for project " + project.getName()) {
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    IWorkspaceDescription wd = ResourcesPlugin.getWorkspace()
                            .getDescription();
                    if (wd.isAutoBuilding()) {
                        project.build(IncrementalProjectBuilder.FULL_BUILD,
                                monitor);
                    }
                } catch (CoreException e) {
                    return new Status(Status.ERROR, JLMP.PLUGIN_ID, -1,
                            "exception caught during build", e);
                }
                return Status.OK_STATUS;
            }
        };
        j.schedule();

    }

    /**
     * 
     */
    private void initOptions() {
        options = new Option[] {
                new SimpleOption(
                        JLMP.PROP_OUTPUT_PROJECT,
                        "Output Project",
                        "The project to which generated sourcecode is written."
                                + "This option is ignored if JTransformer is operating in place.",
                        Option.STRING, JLMPPlugin.getDefault()
                                .getPreferenceValue(
                                        JLMP.PREF_DEFAULT_OUTPUT_PROJECT, null)),
                new SimpleOption(
                        JLMP.PROP_OUTPUT_FOLDER,
                        "Output source folder",
                        "The source folder to which generated sourcecode is written."
                                + "This should be a relative path - it is eather appended to the"
                                + "path of the output project, or - if JTransformer operates inplace -"
                                + " to that of the original project",
                        Option.STRING, JLMPPlugin.getDefault()
                                .getPreferenceValue(
                                        JLMP.PREF_DEFAULT_OUTPUT_FOLDER, null)),
                new SimpleOption(
                        JLMP.PROP_INPLACE,
                        "Try inplace operation",
                        "NOT IMPLEMENTED YET! \nIf this option is set, changes "
                                + "to existing source files will be performed in-place."
                                + "Source files for which the respective PEFs where "
                                + "deleted, are removed. \n"
                                + "Newly created source files however "
                                + "will still be created in the specified output folder.",
                        Option.FLAG, "false"),
                new SimpleOption(
                        JLMP.PROP_PEF_STORE_FILE,
                        "PEF store file",
                        "The file where PEFs for this project should be stored",
                        Option.FILE, JLMPPlugin.getDefault()
                                .getPreferenceValue(
                                        JLMP.PREF_DEFAULT_PEF_STORE_FILE, null)),

        };

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
        PrologInterface pif = getPrologInterface();
        if (pif.isUp()) {
            PrologSession s = pif.getSession();
            try {
                String projectName = getProject().getName();
                String sourceFolder = Util.prologFileName(new File(
                        getPreferenceValue(JLMP.PROP_OUTPUT_FOLDER, null)));
                s.queryOnce("retractall(project_option('" + projectName
                        + "', _))");
            } catch (CoreException e) {
                Debug.report(e);
                throw new RuntimeException(
                        "could not upload output folder location to prolog system",
                        e);
            } finally {
                s.dispose();
            }
        }
        getFactBaseBuilder().clean(null);
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
        PrologInterface pif = getPrologInterface();
        if (pif.isUp()) {
            reconfigure();
        }

    }

    public PrologInterface getPrologInterface() {
        PrologInterface pif = PDTPlugin.getDefault().getPrologInterface(
                getProject().getName());

        if (!pif.isUp()) {
            try {
                pif.start();
            } catch (IOException e) {
                Debug.report(e);
                throw new RuntimeException(e);
            }
        }
        // List libraries = pif.getBootstrapLibraries();
        // ResourceFileLocator l =
        // JLMPPlugin.getDefault().getResourceLocator(JLMP.ENGINE);
        // String name = Util.prologFileName(l.resolve(JLMP.MAIN_PL));
        // if(!libraries.contains(name)){
        // libraries.add(name);
        // if(pif.isUp());
        // PrologSession s = pif.getSession();
        // s.queryOnce("['"+name+"']");
        // }
        return pif;
    }

    /**
     * @return Returns the builder.
     */
    public FactBaseBuilder getFactBaseBuilder() {
        if (builder == null) {
            builder = new FactBaseBuilder(this);
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

    protected void fireFactBaseUpdated() {
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
        Thread t = new Thread(r, "JLMPProject listener notification");
        t.setDaemon(true);
        t.start();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jlmp.JLMPProject#generateFacts(org.eclipse.jdt.core.ICompilationUnit,
     *         java.io.PrintStream)
     */
    public void generateFacts(ICompilationUnit cu, PrintStream out)
            throws IOException, CoreException {
        getFactBaseBuilder().writeFacts(cu, out);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jlmp.JLMPProjectListener#factBaseUpdated(org.cs3.jlmp.JLMPProjectEvent)
     */
    public void factBaseUpdated(JLMPProjectEvent e) {
        fireFactBaseUpdated();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jlmp.JLMPProject#getOptions()
     */
    public Option[] getOptions() {
        if (options == null) {
            initOptions();
        }
        return options;
    }

    /**
     * look up a preference value.
     * <p>
     * tries the following values in the given order and returns the first
     * non-null result. If everything returns null, the given defaultValue is
     * returned.
     * <ul>
     * <li>System.getProperty(key)</li>
     * <li>getProject().getPersistentProperty(key)</li>
     * <li>if an option with the given id exists in the array returned by
     * getOptions(), take its default value</li>
     * <li>the given default value
     * </ul>
     * 
     * @param key
     * @return the value or specified default if no such key exists..
     * @throws CoreException
     */
    public String getPreferenceValue(String key, String defaultValue)
            throws CoreException {
        String value = System.getProperty(key);
        if (value != null) {
            return value;
        }
        value = getProject().getPersistentProperty(new QualifiedName("", key));
        if (value != null) {
            return value;
        }
        Option[] o = getOptions();
        for (int i = 0; i < o.length; i++) {
            if (o[i].getId().equals(key)) {
                return o[i].getDefault();
            }
        }
        return defaultValue;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jlmp.JLMPProject#reconfigure()
     */
    public void reconfigure(PrologSession s) {
        try {
            String projectName = getProject().getName();
            String outputFolder = Util.prologFileName(new File(
                    getPreferenceValue(JLMP.PROP_OUTPUT_FOLDER, null)));
            String outputProject = Util.prologFileName(new File(
                    getPreferenceValue(JLMP.PROP_OUTPUT_FOLDER, null)));
            boolean inplace = Boolean.valueOf(
                    Util.prologFileName(new File(getPreferenceValue(
                            JLMP.PROP_INPLACE, "false")))).booleanValue();

            s.queryOnce("retractall(project_option('" + projectName + "'))");
            s.queryOnce("assert(project_option('" + projectName
                    + "', output_project('" + outputProject + "')))");
            s.queryOnce("assert(project_option('" + projectName
                    + "', output_folder('" + outputFolder + "')))");
            if (inplace) {
                s.queryOnce("assert(project_option('" + projectName
                        + "',inplace))");
            }
        } catch (CoreException e) {
            Debug.report(e);
            throw new RuntimeException(
                    "could not upload output folder location to prolog system",
                    e);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jlmp.JLMPProject#reconfigure()
     */
    public void reconfigure() {
        PrologSession s = getPrologInterface().getSession();
        try {
            reconfigure(s);
        } catch (PrologException e) {
            Debug.report(e);
            throw e;
        } finally {
            s.dispose();
        }
    }

}