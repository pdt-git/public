package org.cs3.pdt.console.internal.views;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.console.internal.ImageRepository;
import org.cs3.pdt.console.internal.actions.QueryConsoleThreadAction;
import org.cs3.pdt.console.internal.actions.QueryMainThreadAction;
import org.cs3.pdt.console.internal.hooks.ConsoleServerHook;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.console.ConsoleModel;
import org.cs3.pl.console.DefaultConsoleController;
import org.cs3.pl.console.DefaultConsoleHistory;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.console.prolog.PrologConsoleEvent;
import org.cs3.pl.console.prolog.PrologConsoleListener;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.part.ViewPart;

public class PrologConsoleView extends ViewPart implements LifeCycleHook, PrologConsole {
    public static final String HOOK_ID = "org.cs3.pdt.console.internal.views.PrologConsoleView";

    private ConsoleViewer viewer;

    private PrologSocketConsoleModel model;

    private DefaultConsoleController controller;

    private PrologCompletionProvider completionProvider;

	private Composite partControl;

	private Vector listeners=new Vector();

	private PrologInterface pif;

	private Menu contextMenu;

	
	
    public PrologConsoleView() {
    }

    public void createPartControl(Composite parent) {
        
    	try {
            createPartControl_impl(parent);
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t.getLocalizedMessage(),t);
        }
    }

    /**
     * @param parent
     */
    private void createPartControl_impl(Composite parent) {
                
        this.partControl=parent;
        
        Listener handler = new Listener(){

			public void handleEvent(Event event) {
				switch(event.type){
				case SWT.Show: 
				case SWT.Hide:
					fireConsoleVisibilityChanged();
					break;
				case SWT.FocusOut:
					fireConsoleLostFocus();
				}
				
			}
        	
        };
		parent.addListener(SWT.Show,handler);
		parent.addListener(SWT.Hide,handler);
		parent.addListener(SWT.FocusOut,handler);
        this.pif = PrologRuntimePlugin.getDefault().getPrologInterface();
        PrologConsolePlugin.getDefault().getPrologConsoleService().registerPrologConsole(this);
        viewer = new ConsoleViewer(parent,SWT.BORDER | SWT.MULTI | SWT.H_SCROLL
				| SWT.V_SCROLL );
        pif.addLifeCycleHook(this, HOOK_ID, new String[] {
                ConsoleServerHook.HOOK_ID});
        
        completionProvider = new PrologCompletionProvider();        
		completionProvider.setMetaInfoProvider(PDTCorePlugin.getDefault().getMetaInfoProvider());
        viewer.setCompletionProvider(completionProvider);
        viewer.setHistory(new DefaultConsoleHistory());
        int port = getPort();
        model = new PrologSocketConsoleModel(false);
        model.setPort(port);
        viewer.setModel(model);
        if (Util.probePort(port)) {
            model.connect();
        }
        initMenus(parent);
        getSite().setSelectionProvider(viewer);
    }

    private IAction[] createActions(){
    	return new IAction[]{
    	new QueryConsoleThreadAction("guitracer","activate guitracer", "activate GUI tracer",
        		ImageRepository.getImageDescriptor(ImageRepository.GUITRACER)),
        new QueryConsoleThreadAction("noguitracer","deactivate guitracer", "deactivate GUI tracer",
        		ImageRepository.getImageDescriptor(ImageRepository.NOGUITRACER)),
        new QueryMainThreadAction("halt","halt", "halt the prolog system",
        		ImageRepository.getImageDescriptor(ImageRepository.BREAK)),
        new Action() {
        	public void run() {
        		try {

                    Job j = new Job("Restarting the PrologInterface") {

                        protected IStatus run(IProgressMonitor monitor) {
                            try {
                                monitor.beginTask("initializing...",
                                        IProgressMonitor.UNKNOWN);

                                try{
                                    pif.stop();
                                }
                                finally{
                                    pif.start();
                                }
                            } catch (Throwable e) {
                                Debug.report(e);
                                return Status.CANCEL_STATUS;
                            } finally {
                                monitor.done();
                            }
                            return Status.OK_STATUS;
                        }
                    };
                    j.schedule();
                } catch (Throwable t) {
                    Debug.report(t);
                }

        	};
        	public org.eclipse.jface.resource.ImageDescriptor getImageDescriptor() {
        		return ImageRepository.getImageDescriptor(ImageRepository.RESTART);
        	};
        	
        	public String getToolTipText() {
        		return "restart";
        	}
        	public String getText() {
        		return "restart";
        	}

        }
        };
    }
    
	private void initMenus(Control parent) {
		IToolBarManager toolbarMenu = getViewSite().getActionBars().getToolBarManager();
        MenuManager manager = new MenuManager();
        manager.setRemoveAllWhenShown(true);
        manager.addMenuListener(new IMenuListener() {
		
			public void menuAboutToShow(IMenuManager manager) {
				fillContextMenu(manager);
				
			}
		
		});
        fillToolbar(toolbarMenu); 
        getSite().registerContextMenu(manager,viewer);
        contextMenu = manager.createContextMenu(parent);
        viewer.getControl().setMenu(contextMenu);
        //ContextMenuProvider menuProvider = new ContextMenuProvider();
		//menuProvider.addMenu(parent);
	}

	private void fillContextMenu(IMenuManager manager) {
		IAction[] actions = createActions();
        for (int i = 0; i < actions.length; i++) {
			IAction action = actions[i];
			
			//menu.add(action);
			manager.add(action);
		}
        manager.add(new Separator
                (IWorkbenchActionConstants.MB_ADDITIONS));
        manager.add(new Separator
                (IWorkbenchActionConstants.MB_ADDITIONS + "-end"));
	}
	private void fillToolbar(IToolBarManager manager) {
		IAction[] actions = createActions();
        for (int i = 0; i < actions.length; i++) {
			IAction action = actions[i];
			
			//menu.add(action);
			manager.add(action);
		}
        manager.add(new Separator
                (IWorkbenchActionConstants.MB_ADDITIONS));
        manager.add(new Separator
                (IWorkbenchActionConstants.MB_ADDITIONS + "-end"));
	}

    private  int getPort() {
        String value = PrologConsolePlugin.getDefault().getPreferenceValue(PDTConsole.PREF_CONSOLE_PORT, null);
        if (value==null) {
            throw new NullPointerException("Required property \""
                    + PDTConsole.PREF_CONSOLE_PORT + "\" was not specified.");
        }
        int port = Integer.parseInt(value);        
         return port;
     }


    public void setFocus() {
        if (viewer == null) {
            Debug
                    .warning("PrologConsoleView.setFocus(): View not instantiated yet.");
            return;
        }
        viewer.getControl().setFocus();
        fireConsoleRecievedFocus();
    }

    private void fireConsoleRecievedFocus() {
		Vector clone =null; 
    	synchronized (listeners) {
    		clone=(Vector) listeners.clone();
		}
    	PrologConsoleEvent e = new PrologConsoleEvent(this);
    	for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = (PrologConsoleListener) iter.next();
			l.consoleRecievedFocus(e);
		}	
	}
    private void fireConsoleLostFocus() {
		Vector clone =null; 
    	synchronized (listeners) {
    		clone=(Vector) listeners.clone();
		}
    	PrologConsoleEvent e = new PrologConsoleEvent(this);
    	for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = (PrologConsoleListener) iter.next();
			l.consoleLostFocus(e);
		}	
	}
    
    private void fireConsoleVisibilityChanged() {
		Vector clone =null; 
    	synchronized (listeners) {
    		clone=(Vector) listeners.clone();
		}
    	PrologConsoleEvent e = new PrologConsoleEvent(this);
    	for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = (PrologConsoleListener) iter.next();
			l.consoleVisibilityChanged(e);
		}	
	}
	public void dispose() {
    	PrologConsolePlugin.getDefault().getPrologConsoleService().unregisterPrologConsole(this);
    	contextMenu.dispose();
    	//viewer.getControl().dispose();
    	super.dispose();
    }
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.LifeCycleHook#onInit(org.cs3.pl.prolog.PrologSession)
     */
    public void onInit(PrologInterface pif,PrologSession initSession) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
     */
    public void afterInit(PrologInterface pif) {
        //viewer.setController(controller);
        model.connect();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
     */
    public void beforeShutdown(PrologInterface pif,PrologSession session) {
        //viewer.setController(null);
        model.disconnect();
    }

	public ConsoleModel getModel() {
		return model;
	}

	public PrologInterface getPrologInterface() {
		return pif;
	}

	public void addPrologConsoleListener(PrologConsoleListener l) {
		synchronized (listeners) {
			if(!listeners.contains(l)){
				listeners.add(l);
			}
		}
		
	}

	public void removePrologConsoleListener(PrologConsoleListener l) {
		synchronized (listeners) {
			if(listeners.contains(l)){
				listeners.remove(l);
			}
		}		
	}

	public boolean isVisible() {
		return partControl.isVisible();
	}

	public ConsoleViewer getViewer() {
		return viewer;
	}

	public String getText() {
		return getViewer().getText();
	}

	public int getLineAtOffset(int offset) {
		return getViewer().getLineAtOffset(offset);
	}

	public int getOffsetAtLine(int line) {
		return getViewer().getOffsetAtLine(line);
	}

	public int getLineCount() {		
		return getViewer().getLineCount();
	}

	public void clearOutput() {
		getViewer().clearOutput();
		
	}

	public String getTextRange(int offset, int length) {
		return getViewer().getTextRange(offset,length);
	}

	public int getCaretOffset() {
		return getViewer().getCaretOffset();
	}
}