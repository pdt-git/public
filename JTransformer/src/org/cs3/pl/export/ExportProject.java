/*
 * Created on 11.09.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.export;

import java.io.IOException;
import java.util.Hashtable;

import org.cs3.pl.PDTPlugin;
import org.cs3.pl.extension.IJTransformerObserver;
import org.cs3.pl.natures.JLMPProjectNature;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.PrologClient;
import org.cs3.pl.prolog.PrologHelper;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;

/**
 * @author rho
 *  
 */
public class ExportProject {

    private IProject project;

    private IProjectDescription description;

    private Object factbaseUpdateMonitor = new Object();

    private int lastStatus;
    
    public ExportProject(IProject project) throws CoreException {
        this.project = project;
        this.description = project.getDescription();
    }

    public void export(final String filename) {
        final String backup = PDTPlugin.getDefault().getStateLocation()
                + "/backup.tmp";

        try {

            final String[] oldNatures = description.getNatureIds();
            setJLMPNature();

            final IPrologClient client = openConnection();
            writeFactbase(client, backup);
            clearFactbase(client);

            Thread thread = new Thread() {
                public void run() {
                    try {

                        synchronized (factbaseUpdateMonitor) {
                            PDTPlugin.getDefault().addJTransformerObserver(new IJTransformerObserver() {

                                public void update(int kind, Object[] info) {
                                    synchronized (factbaseUpdateMonitor) {
                                        lastStatus = kind;
                                        factbaseUpdateMonitor.notifyAll();
                                    }
                                }
                                
                            });
                            project.build(IncrementalProjectBuilder.FULL_BUILD,
                                    null);
                            int status;
                            try {
                                factbaseUpdateMonitor.wait();
                                status = getLastFactbaseUpdateKind();
                            } catch (InterruptedException e) {
                                e.printStackTrace();
                                status = IJTransformerObserver.JT_BUILD_ERROR;
                            }

                            if (status == IJTransformerObserver.JT_BUILD_ERROR)
                                PDTPlugin.getDefault().getWorkbench()
                                        .getDisplay().syncExec(new Runnable() {
                                            public void run() {
                                                MessageDialog
                                                        .openError(PDTPlugin
                                                                .getShell(),
                                                                "Cultivate",
                                                                "Error occured while build. Factbase has NOT been exported.");
                                            }
                                        });
                            else
                                writeFactbase(client, filename);

                        }
                        clearFactbase(client);
                        readFactbase(client, backup);

                        description.setNatureIds(oldNatures);
                        project.setDescription(description, null);
                    } catch (CoreException e1) {
                        e1.printStackTrace();
                    }
                }

                private int getLastFactbaseUpdateKind() {
                    synchronized(factbaseUpdateMonitor){
                        return lastStatus;
                    }
                }
            };
            thread.start();

        } catch (CoreException e) {
            e.printStackTrace();
        }

    }

    /**
     * @param oldNatures
     * @return @throws
     *         CoreException
     */
    private void setJLMPNature() throws CoreException {
        String[] oldNatures = description.getNatureIds();

        if (!description.hasNature(JLMPProjectNature.NATURE_ID)) {
            String[] newNatures = new String[oldNatures.length + 1];
            for (int i = 0; i < oldNatures.length; i++) {
                newNatures[i] = oldNatures[i];
            }
            newNatures[oldNatures.length] = JLMPProjectNature.NATURE_ID;

            description.setNatureIds(newNatures);
            project.setDescription(description, null);
        }
    }

    public boolean clearFactbase(IPrologClient client) {
        return processYesNoQuery(client, "clearTreeFactbase");
    }

    public boolean writeFactbase(IPrologClient client, String filename) {
        return processYesNoQuery(client, "writeTreeFacts('"
                + PrologHelper.makeFilenameSWIConform(filename) + "')");
    }

    public boolean readFactbase(IPrologClient client, String filename) {
        return processYesNoQuery(client, "load_files('"
                + PrologHelper.makeFilenameSWIConform(filename) + "', [])");
    }

    public boolean processYesNoQuery(IPrologClient client, String query) {
        Hashtable table = client.query(query);
        if (table != null) {
            System.err.println("Yes, processing \"" + query + "\"");
        } else {
            System.err.println("No, processing \"" + query + "\"");
        }
        return table != null;
    }

    public IPrologClient openConnection() {
        IPrologClient client = null;
        try {
            client = (PrologClient) PrologManager.getInstance()
                    .getHiddenClient();
            // update(client);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return client;
    }

}