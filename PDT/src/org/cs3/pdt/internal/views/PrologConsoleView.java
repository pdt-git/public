package org.cs3.pdt.internal.views;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PrologConsole;
import org.cs3.pdt.PrologConsoleEvent;
import org.cs3.pdt.PrologConsoleListener;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.QueryConsoleThreadAction;
import org.cs3.pdt.internal.QueryMainThreadAction;
import org.cs3.pdt.internal.actions.RestartAction;
import org.cs3.pdt.internal.hooks.ConsoleServerHook;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.console.ConsoleModel;
import org.cs3.pl.console.ConsoleView;
import org.cs3.pl.console.DefaultConsoleController;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.part.ViewPart;

public class PrologConsoleView extends ViewPart implements LifeCycleHook, PrologConsole {
    public static final String HOOK_ID = "org.cs3.pdt.internal.views.PrologConsoleView";

    private ConsoleView view;

    private PrologSocketConsoleModel model;

    private DefaultConsoleController controller;

    private PrologCompletionProvider completionProvider;

	private Composite partControl;

	private Vector listeners=new Vector();

	private PrologInterface pif;

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
        PDTPlugin plugin = PDTPlugin.getDefault();        
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
        this.pif = plugin.getPrologInterface();
        plugin.getPrologConsoleService().registerPrologConsole(this);
        view = new ConsoleView();
        pif.addLifeCycleHook(this, HOOK_ID, new String[] {
                ConsoleServerHook.HOOK_ID});
        
        controller = new DefaultConsoleController();
        completionProvider = new PrologCompletionProvider();        
		completionProvider.setMetaInfoProvider(plugin.getMetaInfoProvider());
        controller.setCompletionProvider(completionProvider);
        view.setController(controller);
        int port = getPort();
        model = new PrologSocketConsoleModel(false);
        model.setPort(port);
        view.setModel(model);
        view.createPartControl(parent);
        if (Util.probePort(port)) {
            model.connect();
        }
        IToolBarManager toolbarMenu = getViewSite().getActionBars().getToolBarManager();
        IMenuManager menu = getViewSite().getActionBars().getMenuManager();
        
        IAction guitracer = new QueryConsoleThreadAction("guitracer","activate guitracer", "activate GUI tracer",
        		ImageRepository.getImageDescriptor(ImageRepository.GUITRACER));
        IAction noguitracer = new QueryConsoleThreadAction("noguitracer","deactivate guitracer", "deactivate GUI tracer",
        		ImageRepository.getImageDescriptor(ImageRepository.NOGUITRACER));
        IAction breakAction = new QueryMainThreadAction("halt","halt", "halt the prolog system",
        		ImageRepository.getImageDescriptor(ImageRepository.BREAK));
        IAction restart = new Action() {
        	public void run() {
        		(new RestartAction()).runJob();
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

        };
        toolbarMenu.add(restart);
        toolbarMenu.add(breakAction);
        toolbarMenu.add(guitracer);
        toolbarMenu.add(noguitracer);
        menu.add(restart);
        menu.add(breakAction);
        menu.add(guitracer);
        menu.add(noguitracer);
    }


    private  int getPort() {
        String value = PDTPlugin.getDefault().getPreferenceValue(PDT.PREF_CONSOLE_PORT, null);
        if (value==null) {
            throw new NullPointerException("Required property \""
                    + PDT.PREF_CONSOLE_PORT + "\" was not specified.");
        }
        int port = Integer.parseInt(value);        
         return port;
     }


    public void setFocus() {
        if (view == null) {
            Debug
                    .warning("PrologConsoleView.setFocus(): View not instantiated yet.");
            return;
        }
        view.setFocus();
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
    	PDTPlugin.getDefault().getPrologConsoleService().unregisterPrologConsole(this);
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
        view.setController(controller);
        model.connect();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
     */
    public void beforeShutdown(PrologInterface pif,PrologSession session) {
        view.setController(null);
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
}