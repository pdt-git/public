package pdt.y.main;

import java.io.File;
import java.net.MalformedURLException;
import java.util.Set;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.internal.editors.PDTChangedFileInformation;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;


public class GraphPIFCoordinator implements ISelectionChangedListener{
	private static final String FILE_TO_CONSULT = "pl_ast_to_graphML";
	private static final String PATH_ALIAS = "pdt_runtime_builder_graphml_creator";
	private static final String NAME_OF_HELPING_FILE = "pdt-focus-help.graphml";
	  
	private File helpFile;
	private PDTGraphSwingStandalone view;


	public GraphPIFCoordinator(PDTGraphSwingStandalone view) {
		this.view = view;
		PrologRuntimeUIPlugin plugin=PrologRuntimeUIPlugin.getDefault();
		ResourceFileLocator locator = plugin.getResourceLocator();
		helpFile = locator.resolve(NAME_OF_HELPING_FILE);
		//System.out.println("Location of file .graphml file: "+helpFile.toString());		
		PDTPlugin.getDefault().addSelectionChangedListener(this);
	}

//	private void getWorkspaceLocation() {
//		IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
//		workspaceLocation = wsRoot.getLocation().toString();
//		System.out.println(workspaceLocation);
//	}
//	
	
	@Override
	public void selectionChanged(SelectionChangedEvent event) {
		ISelection selection = event.getSelection();
		if (selection instanceof PDTChangedFileInformation) {
			PDTChangedFileInformation fileInfo = (PDTChangedFileInformation)selection;
			queryPrologForGraphFacts(fileInfo.getPrologFileName());
		}

	}
	
	public void queryPrologForGraphFacts(String focusFileForParsing) {
		//PrologInterface pif=PluginActivator.getDefault().getPrologInterface();
		String prologNameOfFileToConsult = PATH_ALIAS+"("+FILE_TO_CONSULT+")";

		try {
			PrologConsole activeConsole = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole();
			if(activeConsole!= null){
				PrologInterface pif = getPifForActiveConsole(activeConsole);
				
				PrologSession session = pif.getSession(PrologInterface.LEGACY);
				session.queryOnce("consult("+prologNameOfFileToConsult+").");
			
			//List<String> folderList/* = collectPrologFilesInWorkspace()*/;
			//folderToParse = convertJavaListToPrologListString(folderList);
			
				String query = "write_focus_to_graphML('"+focusFileForParsing+"','"+Util.prologFileName(helpFile)+"').";
				session.queryOnce(query);
			
				view.loadGraph(helpFile.toURI().toURL());
			}
		} catch (PrologException e1) {
			e1.printStackTrace();
		} catch (MalformedURLException e) {
			e.printStackTrace();
		} catch (PrologInterfaceException e) {
			e.printStackTrace();
		}
	}

	public PrologInterface getPifForActiveConsole(PrologConsole activeConsole) {
		PrologInterface pif = activeConsole.getPrologInterface();
		PrologInterfaceRegistry pifRegistry = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		String pifKey = pifRegistry.getKey(pif);
		Set<Subscription> subscriptions = pifRegistry.getSubscriptionsForPif(pifKey);
		
		if (ownSubscriptionMissing(subscriptions)) {
			FocusViewSubscription mySubscription = FocusViewSubscription.newInstance(pifKey);
			pif = PrologRuntimeUIPlugin.getDefault().getPrologInterface(mySubscription);
		}
		return pif;
	}
	
	private boolean ownSubscriptionMissing(Set<Subscription> subscriptions) {
		for (Subscription subscription : subscriptions) {
			if (subscription instanceof FocusViewSubscription)
				return false;
		}
		return true;
	}
	
//	private String selectFolderToParse() {		
//		Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
//		// File standard dialog
//		DirectoryDialog folderDialog = new DirectoryDialog(shell);
//		folderDialog.setText("Select Folder");
//		// Open Dialog and save result of selection
//		String selectedPath = folderDialog.open();
//		File file = new File(selectedPath);
//		String prologNameOfSelectedPath = Util.prologFileName(file);
//		
//		return prologNameOfSelectedPath;
//	}
//	
//	private List<String> collectPrologFilesInWorkspace() {
//		final List<String> fileList = new ArrayList<String>();
//		try {
//			ResourcesPlugin.getWorkspace().getRoot().accept(new IResourceVisitor(){
//
//				@Override 
//				public boolean visit(IResource resource) throws CoreException {
//					if (resource instanceof IProject && resource.isAccessible()) {
//						IPath path = resource.getLocation();
//						File file = path.toFile();
//						
//						fileList.add(Util.prologFileName(file));
//						System.out.println("Folder: "+file.toString());
//						return false;
//					}
//					return true;
//				}
//			});
//		} catch (CoreException e) {
//		};
//		System.out.println("fertig mit sammeln");
//		return fileList;
//	}
	
//	private String convertJavaListToPrologListString(List<String> stringList) {
//		StringBuffer buffer = new StringBuffer();
//		for (String elem : stringList) {
//			buffer.append(elem);
//			buffer.append("','");
//		}
//		buffer.delete(buffer.length()-3, buffer.length());
//		System.out.println("String to write:"+buffer.toString());
//		return buffer.toString();
//	}

}
