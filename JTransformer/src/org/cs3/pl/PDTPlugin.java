/*
 * Created on 28.01.2004
 *
 */
package org.cs3.pl;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringBufferInputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Vector;

import org.cs3.pl.builders.FactBaseBuilder;
import org.cs3.pl.builders.JLMPProjectBuilder;
import org.cs3.pl.editors.PLEditor;
import org.cs3.pl.extension.IJTransformerObserver;
import org.cs3.pl.natures.JLMPProjectNature;
import org.cs3.pl.prolog.CacheOutputPrologListener;
import org.cs3.pl.prolog.FactBaseInitialization;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.PrologManager;
import org.cs3.pl.prolog.SourceLocation;
import org.cs3.pl.views.PrologConsole;
import org.eclipse.core.internal.localstore.FileSystemResourceManager;
import org.eclipse.core.internal.resources.ProjectDescription;
import org.eclipse.core.internal.resources.Workspace;
import org.eclipse.core.internal.utils.Assert;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.ISavedState;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IPluginDescriptor;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkingSetManager;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.texteditor.AbstractTextEditor;
import org.osgi.framework.BundleContext;
//import ca.ubc.jquery.preferences.JQueryPreferencePage;
//import ca.ubc.jquery.engine.WorkingSetRuleBaseMapper;


/**
 * @author windeln
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PDTPlugin extends AbstractUIPlugin  implements ISaveParticipant  {
	
	/** The id of the plugin. */
	public static final String PLUGIN_ID = "org.cs3.pl";
	
	/** The name of the plugin */
	public static final String PLUGIN_NAME = "PDT";

	/** The version of the plugin */
	public static final String PLUGIN_VERSION = "PDT VERSION 0_0_1";
	
	/** The path to the plugin install directory */ 
	public static File installPath = null;
	
	/** True if the tracing option ca.ubc.jquery/debug/ui is on. */
	private static boolean isDebuggingUI = true;

	/** True if the tracing option ca.ubc.jquery/debug/queries is on. */
	private static boolean isDebuggingQueries;

	/**True if user's actions are to be logged */
	private static boolean isLoggingUser;

	/** The shared instance. */
	private static PDTPlugin plugin = null;
	
	/** Path to icons folder relative to installPath */
	private static final String iconPath = "icons/";
	
	

	private PrologConsole prologConsole;

	//private IProject currentProject;
	
	private NullProgressMonitor monitor = new NullProgressMonitor();

	//private IWorkspaceRoot root;

	private static final String SWIPROJECTNAME = "swipl";

	private CacheOutputPrologListener cache;

	private HashMap builders = new HashMap();

	public static final String TRANSFORMED = "Transformed";

	public static final String JTRANFORMERENGINE = "JTransformer Engine";

	public static final String MODULEPREFIX = "pdtplugin:";

    private boolean cachingJavaLangEnabled = false;

    private boolean cachingCompleteEnabled = false;

    private boolean singletonDTM = false;

    public static final int DEFAULTPORT = 8807;
    
    private int prologServerPort = DEFAULTPORT;

    final protected String CLASSPATHXML = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<classpath>\n<classpathentry kind=\"src\" path=\"\"/>\n<classpathentry kind=\"con\" path=\"org.eclipse.jdt.launching.JRE_CONTAINER\"/>\n<classpathentry kind=\"output\" path=\"\"/>\n</classpath>";	
	
    private Vector observers = new Vector();
	/** The rule bases. */
//	private static WorkingSetRuleBaseMapper ruleBaseMapper = null;

	/** Path to the common user rules file (applies to all projects) */
	
	/**
	 * The constructor.
	 */
	public PDTPlugin(IPluginDescriptor d) {
		super(d);
		plugin = this;

		// get the preferences from the store
		updatePreferences();
		
		//initialize log of user's actions 
		
		// Set tracing flags
		
		
		// Have to do this after getting install directory because
		// WorkingSetRuleBaseMapper depends on it.
		
		// Load images
//		ImageRegistry imgReg = getImageRegistry();
//		imgReg.put("String", getImageDescriptor("String.gif"));

		//shell = getWorkbench().getActiveWorkbenchWindow().getShell();
		//activeWorkspace = getWorkbench().getActiveWorkbenchWindow();
		
//		createMenu();
		
	}

	/**
	 * Returns the ImageDescriptor of the file at the given location
	 * relative to the plugin's icon directory.
	 */
	public static ImageDescriptor getImageDescriptor(String name) {
		
		try {
			URL installURL = getDefault().getBundle().getEntry("/");
			URL url = new URL(installURL, iconPath + name);
			return ImageDescriptor.createFromURL(url);
		} catch (MalformedURLException e) {
			// should not happen
			return ImageDescriptor.getMissingImageDescriptor();
		}
	}
	
	/**
	 * Returns the shared instance.
	 */
	public static PDTPlugin getDefault() {
		return plugin;
	}

	/**
	 * Displays an error message to the user.
	 */
	public static void error(Object msg) {
		if (msg != null) {
			MessageDialog.openError(getShell(), PLUGIN_NAME, msg.toString());
		}
	}
	
	/**
	 * Displays an information message to the user.
	 * @author wannop
	 */
	public static void message(Object msg) {
		if (msg != null) {
			MessageDialog.openInformation(getShell(), PLUGIN_NAME, msg.toString()); 
		}
	}
	
	/**
	 * Outputs message to System.out if isDebuggingUI is true.
	 */
	public static void traceUI(Object message) {
		trace(isDebuggingUI,message);
	}

	/**
	 * Outputs message to System.out if isDebuggingQueries is true.
	 */
	public static void traceQueries(Object message) {
		trace(isDebuggingQueries,message);
	}

	private static void trace(boolean traceFlag,Object message) {
		if (traceFlag || message instanceof Throwable) {
			if (message == null) {
				Debug.debug("["+PLUGIN_NAME+"] null");
			}
			else {
				Debug.debug("["+PLUGIN_NAME+"] "+getMessage(message));
			}
		}
	}
	
	private static String getMessage(Object message) {
		if (message instanceof Throwable) {
			ByteArrayOutputStream byte_out = new ByteArrayOutputStream();
			PrintWriter out = new PrintWriter(byte_out);
			Debug.report((Throwable)message);
			out.close();
			return byte_out.toString();
		}
		else
			return message.toString();
	}


	public void stop(BundleContext c) throws Exception {
		FactBaseInitialization initfactbase = 
		    new FactBaseInitialization(PrologManager.getInstance().getHiddenClient());
		initfactbase.onPluginStop();
		super.stop(c);
		savePluginPreferences();
	}

	
	
	public static Shell getShell() {
		try {
			return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		}catch (NullPointerException e){
			return null;
		}
	}
	
    protected void initializeDefaultPreferences(IPreferenceStore store) {
		store.setDefault(PDTPreferencePage.P_JAVA_LANG_CACHING, false);
		store.setDefault(PDTPreferencePage.P_COMPLETE_CACHING, false);
		store.setDefault(PDTPreferencePage.P_SINGLETON_DTM, false);
		store.setDefault(PDTPreferencePage.P_PROLOG_SERVER_PORT, PrologManager.getServerPort());
	} 
    
	public void updatePreferences() {
		IPreferenceStore store = getPreferenceStore();
		
		// update all the variables based on the new values in the preference store
		cachingCompleteEnabled = store.getBoolean(PDTPreferencePage.P_COMPLETE_CACHING);
		cachingJavaLangEnabled = store.getBoolean(PDTPreferencePage.P_JAVA_LANG_CACHING);
		singletonDTM = store.getBoolean(PDTPreferencePage.P_SINGLETON_DTM);

		PrologManager.setServerPort(store.getInt(PDTPreferencePage.P_PROLOG_SERVER_PORT));
		System.out.println("");
	}

	/**
	 * @param string
	 * @param e
	 */
	public static void error(String msg, Throwable e) {
//      ByteArrayOutputStream stackTrace = new ByteArrayOutputStream();
//      e.printStackTrace(new PrintWriter(stackTrace));
		Debug.report(e);
		error(msg+ e.getMessage());
	}


	public void doneSaving(ISaveContext context) {
		// delete the old saved state since it is not necessary anymore
		String oldFileName = "save-" + Integer.toString(context.getPreviousSaveNumber());
		File f = plugin.getStateLocation().append(oldFileName).toFile();
		f.delete();
	}

	public void prepareToSave(ISaveContext context) {
	}

	public void rollback(ISaveContext context) {
	}

	public void saving(ISaveContext context) {
		context.needDelta(); //need this for workingSetManager
		//example code  
//      String saveFileName = "save-" + Integer.toString(context.getSaveNumber());
//      File f = plugin.getStateLocation().append(saveFileName).toFile();
//      plugin.writeImportantState(f);
//      
//      context.map(new Path("save"), new Path(saveFileName));
//      context.needSaveNumber();
	}
	
	/* (non-Javadoc)
	org.eclipse.core.runtime.Plugin#start(BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		ISavedState ss = ResourcesPlugin.getWorkspace().addSaveParticipant(this, this);
		//root = ResourcesPlugin.getWorkspace().getRoot();
		String location;
		//FIXED: ld: this should be set according to some env. var. or sys. prop.
		//Resolved: debug level is now initialized from the system property
		//       "org.cs3.pl.jtransformer.debug_level"
		//see the static block of the Debug class.
		//Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		
		try {
			location = getLocation()+ SWIPROJECTNAME;
			addProject(SWIPROJECTNAME,new Path(getCanonicalPath(location)));
			location = getLocation()+ "engine";
			
			addProject(JTRANFORMERENGINE,new Path(getCanonicalPath(location)));
			
			addProject(TRANSFORMED,new Path(TRANSFORMED));
			
//			WorkspaceJob job = new WorkspaceJob("starting prolog server") {
//				public IStatus runInWorkspace(IProgressMonitor monitor)
//						throws CoreException {
////			Thread.sleep(500);
//					return Status.OK_STATUS;
//				}
//			};
//			job.schedule();

		} catch (FileNotFoundException e) {
			Debug.report(e);
		} catch (IOException e2) {
			Debug.report(e2);
		}
		
	}

	/**
	 * @return
	 * @throws IOException
	 */
	public String getWorkspaceLocation() throws IOException {
		String path = ResourcesPlugin.getWorkspace().getRoot().getLocation().toFile().getCanonicalPath();
		if(path.charAt(path.length()-1) == File.separatorChar)
			return path;
		return path + File.separator;
	}

	/**
	 * @param location
	 * @return
	 * @throws IOException
	 */
	private String getCanonicalPath(String location) throws IOException {
		return (new java.io.File(location)).getCanonicalPath();
	}

	/**
	 * 
	 */
	
	/**
	 * Creates a new project at a specific location, if the project does not exist yet.   
	 * 
	 * @param projectName name of the project.
	 * @param postfix project location.
	 * @throws CoreException
	 */
	
	public void addProject(final String projectName, final IPath location)  {
		WorkspaceJob job = new WorkspaceJob("create project: " + projectName) {
			public IStatus runInWorkspace(IProgressMonitor monitor)
					throws CoreException {
				try {
					IProject project = getWorkspace().getRoot().getProject(
							projectName);
					
					if (!project.exists()) {
						Debug.info("creating project: " + projectName
								+ " in location " + location.toString());
						if (location.isAbsolute()) {
							IProjectDescription desc = new ProjectDescription();
							desc.setLocation(location);
							desc.setName(projectName);
							project.create(desc, monitor);
							project.open(monitor);
						} else {
							project.create(monitor);
							getRoot()
									.refreshLocal(IResource.DEPTH_ONE, monitor);
							try {
								project.open(monitor);
								getRoot().refreshLocal(
										IResource.DEPTH_INFINITE, monitor);
							} catch (CoreException e) {
								Debug.report(e);
							}

							IProjectDescription description = project
									.getDescription();
							String[] natureIds = description.getNatureIds();
							String[] newNatureIds = new String[natureIds.length + 1];
							System.arraycopy(natureIds, 0, newNatureIds, 0,
									natureIds.length);
							newNatureIds[natureIds.length] = JavaCore.NATURE_ID;

							description.setNatureIds(newNatureIds);
							writeClasspathFile(project);
							project.setDescription(description,
									IResource.FORCE, null);
						}
						 
						
						getRoot().refreshLocal(IResource.DEPTH_INFINITE,
								monitor);
					}
					if (!project.isAccessible()) {
						try {
							project.open(monitor);
							getRoot().refreshLocal(IResource.DEPTH_INFINITE,
									monitor);
						} catch (CoreException e) {
							Debug.report(e);
						}
					}
					return Status.OK_STATUS;
				} catch (CoreException ex) {
					Debug.report(ex);
					return Status.CANCEL_STATUS;
				} catch (IOException e) {
					Debug.report(e);
					return Status.CANCEL_STATUS;
                }

			}

            /**
             * @param project
             * @throws IOException
             */
            private void writeClasspathFile(IProject project) throws IOException {
                File classpathFile = new File(project.getLocation().toOSString() + File.separator + ".classpath");
                BufferedWriter writer = new BufferedWriter(new FileWriter(classpathFile));
                writer.write(CLASSPATHXML);
                writer.close();
            }
		};
		job.schedule();

	}

	/**
	 * @return
	 */
	private IWorkspaceRoot getRoot() {
		try {
			return getWorkspace().getRoot();
		}catch (NullPointerException e){ return null; }
	}

	/**
	 * @return
	 */
	public IWorkingSetManager getWorkingSetManager() {
		try {
			return getDefault().getWorkbench().getWorkingSetManager();
		}catch (NullPointerException e){ return null; }
	}


	IWorkbenchWindow activeWorkbench;

	public IWorkbenchWindow getActiveWorkbenchWindow() {
		if (activeWorkbench == null){
			getDisplay().syncExec(new Runnable() {
				public void run() {
					activeWorkbench = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
				}
			});
		}
		return activeWorkbench;
//		try {
//		return PlatformUI.getWorkbench().getActiveWorkbenchWindow();
//		}catch (NullPointerException e){ return null; }
		
	}

	public Workspace getWorkspace() {
		return (Workspace)ResourcesPlugin.getWorkspace();
	}
	/**
	 * @return
	 */
	public IPrologClient getPrologClient() {
		//activatePrologConsole();
//		if(Display.getCurrent()==null){
//			//ld: _*NOT*_ asyncExec!
//			getDisplay().syncExec(new Runnable(){
//				public void run() {
//					activatePrologConsole();
//				}
//			});
//		}
//		else{
//			activatePrologConsole();
//		}
		return PrologManager.getInstance().getClient();
	}

	/**
	 * @return
	 */
	public PrologConsole getPrologConsole() {
		if (prologConsole != null)
		return prologConsole;
		
		//activatePrologConsole();
		return prologConsole;
	}
		
	public void setPrologConsole(PrologConsole prologConsole) {
		this.prologConsole = prologConsole;
	}
	
	public void resetConsole() {
		if (prologConsole != null)
			prologConsole.reset();
	}

	public IWorkbenchPage getActivePage() {
		try {
			return getActiveWorkbenchWindow().getActivePage();
		}catch (NullPointerException e){
			return null;
		}
			
	}

/*
 * LD: FIXME: TODO: NOTE: ATTENTION: ACHTUNG: AUFGEPASST: HERGEH?RT:
 * There is no such thing as an "active" or "current" project in eclipse.
 * if some other method needs the project to which the file in the current
 * active editor belongs to, it should _EXPLICITLY_ ask for that file and call
 * getProject on it. This is not that difficult, and possible everywhere: 
 * e.g. 
 * 
 * PDTPlugin.getDefault().getActiveFile().getProject()
 * 
 * There is no way the Plugin class can determine the respective project 
 * any way better than you can yourself. Implying otherwise (by adding methods like 
 * getProject() to this class) means asking for trouble. 
 * Just consider the ByteCodeFactGenerator, which currently asks the PDTPlugin for the 
 * "current project", so he can use its java nature to find class files. This IS PLAIN WRONG. 
 * If the results look right every now and then, then this is purely a matter of lucky 
 * coincidence.
 * The plugin does not know about the project the ByteCodeFactGenerator might be interested in.
 * Nore does the plugin know which classpath entries to concider for resolving FQNs.
 * This is context-dependent information, which should be achieved by other means.
 * e.g. the builder could initialize the bcfg with the project (or classpath there of) it 
 * belongs to. (each FactbaseBuilder is associated to exactly one project via its JLMP nature)   
 * 
 * If some other code actualy needs this mysterious "Current Project(tm)", this code 
 * IS WRONG and SHOULD BE FIXED. period.
 */	

//	/**
//	 * Returns the corresponding project of the file 
//	 * in the active editor. 
//	 * 
//	 * @return null, if there is no active editor.
//	 */
//	
//	public IProject getProject() {
//		try {
//			IFile input =((IFileEditorInput)getActiveWorkbenchWindow().getActivePage().getActiveEditor().getEditorInput()).getFile();
//			return input.getProject(); // <-- das zum File geh?rende Projekt
//		} catch (Exception exception) {
//			throw new RuntimeException("Can't determine Project.");
//		}
//	}
	
//	public String getProjectPath() { 
//		File workspacePath = getProject().getWorkspace().getRoot().getRawLocation().toFile();
//		return workspacePath.toString().replace(File.separatorChar, '/') + '/'+ getProject().getName();
//	}	

//	public String getProjectPath(IProject project) { 
//		File workspacePath = project.getWorkspace().getRoot().getLocation().toFile();
//		return workspacePath.toString().replace(File.separatorChar, '/') + '/'+ getProject().getName();
//	}	
	
	public static IEditorPart openInEditor(IFile file, boolean activate) throws PartInitException {
		if (file != null) {
			IWorkbenchPage p= getDefault().getActivePage();
			if (p != null) {
				IEditorPart editorPart= IDE.openEditor(p, file, activate);
//				initializeHighlightRange(editorPart);
				return editorPart;
			}
		}
		return null; 
	}

	public IFile getActiveFile() {
		try {
			IEditorPart editorPart = getActiveEditor();
			return ((FileEditorInput)editorPart.getEditorInput()).getFile();
		}catch (NullPointerException e){ return null; }
	}

	public String getActiveFileName() {
		try {
			return getActiveFile().getFullPath().toString();
		}catch (NullPointerException e){ return null; }
	}
	
	
	public String getActiveRawFileName() {
		try {
			return getActiveFile().getRawLocation().toFile().getAbsolutePath();
		}catch (NullPointerException e){ return null; }

	}
	/**
	 * @return
	 */
	public IEditorPart getActiveEditor() {
		try {		
			IWorkbenchPage page = getActiveWorkbenchWindow().getActivePage();
			return page.getActiveEditor();
		}catch (NullPointerException e){ return null; }
	}

	/**
	 * @return
	 */
	public FileSystemResourceManager getFileSystemManager() {
		try {
			return getWorkspace().getFileSystemManager();
		}catch (NullPointerException e){ return null; }
	}

	/**
	 * 
	 */
	public Display getDisplay() {
		try {
			return getWorkbench().getDisplay();
		}catch (NullPointerException e){ return null; }
	}

	public void gotoLocation(SourceLocation location) {
		String filename = location.file;
		Debug.debug("gotoLocation got this filename: "+filename);
		try {
			filename = (new File(filename).getCanonicalPath());
		} catch (IOException e3) {
			Debug.report(e3);
		}
		Debug.debug("and now, gotoLocation got this filename: "+filename);		
		IPath path = new Path(filename);
//		FileLocationRetriever retriever  = new FileLocationRetriever(PDTPlugin.getDefault());
		IFile file = PDTPlugin.getDefault().getFileSystemManager().fileForLocation(path);
//		file = retriever.fileForLocation(path);
		if(file == null) {//FIXME: if the file is no a full path
			file = PDTPlugin.getDefault().getWorkspace().getRoot().getFile(new Path(location.file));
			if(!file.exists())
				file = null;

		}
		if(file == null) {
			MessageDialog.openError(getShell(),"PDT Plugin", "can't find the file '" + filename + "' in the workspace.");
			return;
		}
		
		IWorkspaceRoot o = ResourcesPlugin.getWorkspace().getRoot();
		if(! file.isSynchronized(IResource.DEPTH_ZERO)){
			Debug.info("Synchronizing resource: "+file);
			try {
				file.refreshLocal(IResource.DEPTH_ZERO,new NullProgressMonitor());
			} catch (CoreException e) {
				Debug.report(e);
				MessageDialog.openInformation(getWorkbench().getActiveWorkbenchWindow().getShell(),"PDT Plugin", e.getLocalizedMessage());
			}
		}
		
		IWorkbenchPage page= PDTPlugin.getDefault().getActivePage();
		IEditorInput input= new FileEditorInput(file);//createEditorInput(file));
		String editorId = null; 
			editorId =
				PDTPlugin
					.getDefault()
					.getWorkbench()
					.getEditorRegistry()
					.getDefaultEditor(path.toFile().getAbsolutePath())
					.getId();
			
		
		try {
			page.openEditor(input, editorId, true);
		} catch (Exception e1) {
			Debug.report(e1);
		}
		
		while(!((FileEditorInput)PDTPlugin.getDefault().getActiveEditor().getEditorInput()).getFile().equals(file))
			try {
				Thread.sleep(50);
			} catch (InterruptedException e2) {
				Debug.report(e2);
			}
		((PLEditor)PDTPlugin.getDefault().getActiveEditor()).gotoLine(location.line);
	}

	/**
	 * @param string
	 */
	public void openWarning(final String string) {
		getDisplay().syncExec(new Runnable() {
			/* (non-Javadoc)
			 * @see java.lang.Runnable#run()
			 */
			public void run() {
				MessageDialog.openWarning(getShell(),"PDTPlugin",string);
			}
		});

	}

//	/**
//	 * @param project
//	 */
//	public void setCurrentProject(IProject project) {
//		currentProject = project;
//	}
//
//	public IProject getCurrentProject() {
//		return currentProject;
//	}

	/**
	 * retrieve the location where fact files for this projectg's source
	 * are stored.
	 * 
	 * This will typicaly be within the project directory. In every case,
	 * the returned location is guarantied to be unique for all projects.
	 * 
	 * @param project
	 * @return the path where the fact files for the project source 
	 * 		  are stored
	 */
	public IPath getProjectFactsPath(IProject project) {
		return project.getFullPath().append("pl").makeRelative();		
	}
	/**
	 * retrieve the location where bytecode facts are stored.
	 * 
	 * This path is guaranteed to be <b>outside</b> the path returned
	 * by getProjectFactsPath(IProject) for the same project.
	 * It is, n.b., <b>not</b> guaranteed to be unique, i.e.,  
	 * the implementation may share bytecode facts
	 * among different projects to gain performance.
	 * 
	 * @param project
	 * @return the path where the fact files for external bytecode
	 * 		   are stored.
	 */
	public IPath getExternalFactsPath(IProject project) {
		return project.getFullPath().append("ext").makeRelative();		
	}

	public TextSelection getSelection() {
		IEditorPart editor = PDTPlugin.getDefault().getActivePage().getActiveEditor();
		return (TextSelection)editor.getEditorSite().getSelectionProvider().getSelection();
	}

	/**
	 * @param string
	 */
	public void setStatusErrorMessage(final String string) {
		getDisplay().asyncExec(new Runnable() {
			public void run() {
				getActiveEditor().getEditorSite().getActionBars().getStatusLineManager().setErrorMessage(string);
			}
		});
	}

	public void setStatusMessage(final String string) {
		getDisplay().asyncExec(new Runnable() {
			public void run() {
				getActiveEditor().getEditorSite().getActionBars().getStatusLineManager().setMessage(string);
			}
		});
	}
 
	/**
	 * @return
	 */
//	public PrologManagerClient getHiddenPrologManager() throws IOException {
//				
////		if(Display.getCurrent()==null){
////			//ld: _*NOT*_ asyncExec!
////			getDisplay().syncExec(new Runnable(){
////				public void run() {
////					activatePrologConsole();
////				}
////			});
////		}
////		else{
////			activatePrologConsole();
////		}
//		return PrologManagerClient.getHiddenInstance();
//	}

	/**
	 * @param start
	 * @param length
	 * @param filename
	 */
	public void selectInEditor(int start, int length, String filename) throws PartInitException {
		Path path = new Path(filename);
		IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(path);
		if (file == null) {
			PDTPlugin.getDefault().setStatusErrorMessage("could not find the file: '" + filename + "' in the workspace.");
			return;
		}
		openInEditor(file, true);
		IDocument document = ((AbstractTextEditor)getActiveEditor()).getDocumentProvider().getDocument(getActiveEditor().getEditorInput());
		ISelection selection = new TextSelection(document,start,length);
		getActiveEditor().getEditorSite().getSelectionProvider().setSelection(selection);
	}

	/**
	 * @return
	 */
	public String getLocation() throws FileNotFoundException {
		URL url = getBundle().getEntry("/");
		String location = null;
		try {
			location = new File(Platform.asLocalURL(url).getFile()).getAbsolutePath();
		} catch (IOException e) {
			throw new FileNotFoundException("The installation directory '" + location + "' of the plugin could not be found.");
			//FIXME ld:we should simply throw an IOException.
		}
		if (location.charAt(location.length()-1)!= File.separatorChar)
			location += File.separatorChar;
		return location;
	}

	public IFile getFile(IPath path) {
		IWorkspaceRoot o = ResourcesPlugin.getWorkspace().getRoot();
		IFile file = o.getFile(path);
		if (!file.isSynchronized(IResource.DEPTH_ZERO)) {
			//System.err.println("Synchronizing resource: " + file);
			try {
				file.refreshLocal(IResource.DEPTH_ZERO,
						new NullProgressMonitor());
			} catch (CoreException e) {
				Debug.report(e);
			}
		}
		return file;
	}

	
	/**
	 * Ensures that after this call, isAccessible() will return true for this
	 * resource.
	 * 
	 * @param r a resource.
	 */
	public void ensureAccessible(IResource r) throws CoreException {
		if (!r.isSynchronized(IResource.DEPTH_ZERO)) {
			r.refreshLocal(IResource.DEPTH_ZERO, null);
		}
		if (r.isAccessible())
			return;
		boolean exists = r.exists();
		IResource p = r.getParent();
		//the parent should in no case be null, since the ws root
		//does always exist. This is sheer paranoia:
		if (p == null) {			
			Debug.error("Weird things are about to happen. :-(");
		}
		ensureAccessible(p);
		switch (r.getType()) {
			case IResource.FILE :
				IFile file = (IFile) r;
				if (!exists)
					file.create(new StringBufferInputStream(""), true, null);
				break;
			case IResource.FOLDER :
				IFolder folder = (IFolder) r;
				if (!exists)
					folder.create(true, true, null);
				break;
			case IResource.PROJECT :
				IProject project = (IProject) p;
				if (!exists)
					project.create(null);
				project.open(null);
				break;
			case IResource.ROOT :
				Debug.error("PrologFactBuilder: Punk! Weirdo! Go away!");
				break;
		}
		if (!r.isSynchronized(IResource.DEPTH_ZERO)) {
			r.refreshLocal(IResource.DEPTH_ZERO, null);
		}
		if (!r.isAccessible()) {
			Debug.error("PrologFactBuilder: Housten, we have a problem!");
		}
	}	
		/**
	 * retrieve the fact builder for a given project.
	 * <p>
	 * Builder instances are created lazily when first requested. 
	 * Although instances are typicaly hold in cache, the calling code should <b>not</b>
	 * assume object identity of the returned object, <b>not even</b> when calling 
	 * this method several times for the same project.
	 *    
	 * @param project the project to which the builder is associated. 
		 * @throws IOException
	 */
	public FactBaseBuilder getBuilder(IProject project) throws IOException {
		Object result = builders.get(project);
		if (result==null){
			result = new FactBaseBuilder(project, PrologManager.getInstance().getHiddenBuilderClient());
			builders.put(project,result);
		}
		return (FactBaseBuilder)result;				
	}	
	/**
	 * (re)initialize factbase for open projects.
	 */
	public void initializeOpenProjects() {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		try {
			root.accept(new IResourceVisitor(){

				public boolean visit(IResource resource) throws CoreException {
					switch(resource.getType()){
						case IResource.ROOT: return true;
						case IResource.PROJECT:
							IProject p = (IProject) resource;
							if (p.isOpen() && p.hasNature(JLMPProjectNature.NATURE_ID)) {
								buildJLMPProject(p);
							}						
							break;
						default: return false;
					}
					return false;
				}
				
			});
		} catch (CoreException e) {
			Debug.report(e);
		}
	}
	/**
	 * Called when a JLMP project is opened.
	 * FIXME: This should NOT be public
	 * <p>
	 * 
	 * @param p
	 */
	public void buildJLMPProject(final IProject p) {
		
			//p.build(IncrementalProjectBuilder.AUTO_BUILD,null);
			//FIXME: async job neccessary? (ld)
			Job j = new Job("JLMP Builder Job"){
				protected IStatus run(IProgressMonitor monitor) {
					try {
						//JLMPProjectNature nature = (JLMPProjectNature) p.getNature(JLMPProjectNature.NATURE_ID);
						//nature.getBuilder().build(null,0,monitor);
						p.build(IncrementalProjectBuilder.FULL_BUILD,JLMPProjectBuilder.BUILDER_ID,null,null);
					} catch (Throwable e) {//well... uh.....eclipse eats my exceptions :-( 
						Debug.report(e);
						return Status.CANCEL_STATUS;
					}
					
					return Status.OK_STATUS;
				}						
			};			
			j.schedule();
		
	}

	/**
	 * @param icu
	 * @return
	 */
	/**
	 * @param icu
	 * @return
	 */
	//ld: migration to m9 requires this:
	//ASTParser parser = ASTParser.newParser(AST.LEVEL_2_0);  
	ASTParser parser = ASTParser.newParser(AST.JLS2);

    private static final String EXTENSIONPOINTFACTBASEUPDATED = "factbase.observer";

    private static final Object observerMonitor = new Object();
	
	public CompilationUnit parseICompilationUnit(ICompilationUnit icu) {
		parser.setSource(icu);
		parser.setResolveBindings(true);
		CompilationUnit root=(CompilationUnit)parser.createAST(null);
		return root;
	}

	public CompilationUnit parseICompilationUnit(String s) {
		parser.setSource(s.toCharArray());
		parser.setResolveBindings(true);
		CompilationUnit root=(CompilationUnit)parser.createAST(null);
		return root;
	}

	/**
	 * @return
	 */
	public static String getClasspath() {
		String proj;
		String cp;
		
		
		String resName = "org/cs3/pl/PDTPlugin.class";
		String path = ClassLoader.getSystemClassLoader().getResource(resName).getFile();
		
		return path.substring(0, path.indexOf(resName));
	}

    public boolean isJavaLangCachingEnabled() {
        return cachingJavaLangEnabled;
    }

    public boolean isCompleteCachingEnabled() {
        return cachingCompleteEnabled;
    }

    /**
     * Looks up all avaible extensions for the extension point  
     * org.cs3.pl.extension.factbase.updated, creates Observer objects
     * and calls their update() methods. 
     * @param prologManager
     * 
     * @throws CoreException
     */
    public boolean updateFactbaseObservers(int kind,IPrologClient prologClient) {
        Vector copy;
        synchronized(observerMonitor) {
            copy = new Vector(observers);
        }
        for (int i = 0; i < observers.size(); i++) {
            ((IJTransformerObserver) copy.get(i)).update(kind,
                    new Object[] { prologClient });
        }
        
        
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint(
                "org.cs3.pl.JTransformer", EXTENSIONPOINTFACTBASEUPDATED);
        if (point == null) {
            Debug.error("could not find the extension point "
                    + EXTENSIONPOINTFACTBASEUPDATED);
            return false;
        }
        IExtension[] extensions = point.getExtensions();
        try {
            for (int i = 0; i < extensions.length; i++) {
                IConfigurationElement[] celem = extensions[i]
                        .getConfigurationElements();
                Assert.isTrue(celem[0].getName().equals("observer"));
                IJTransformerObserver observer = (IJTransformerObserver) celem[0]
                        .createExecutableExtension("class");
                observer.update(kind,
                        new Object[] { prologClient });
            }
        } catch (CoreException e) {
            Debug.report(e);
            return false;
        }
        return true;
    }
    
    public void addJTransformerObserver(IJTransformerObserver observer){
        synchronized(observerMonitor) {
            observers.add(observer);
        }
    }

    public void removeJTransformerObserver(IJTransformerObserver observer){
        synchronized(observerMonitor) {
        observers.remove(observer);
        }
    }

    public boolean singletonCheckForDTMVars() {
        return singletonDTM;
    }
    /**
     * @return Returns the prologServerPort.
     */
    public int getPrologServerPort() {
        return prologServerPort;
    }
}
