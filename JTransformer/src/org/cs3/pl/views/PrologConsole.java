package org.cs3.pl.views;
import java.io.IOException;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import jpl.Atom;
import jpl.Compound;
import jpl.Term;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.editors.PLEditor;
import org.cs3.pl.prolog.IMetaInfoProvider;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.IPrologListener;
import org.cs3.pl.prolog.PrologElementData;
import org.cs3.pl.prolog.PrologEvent;
import org.cs3.pl.prolog.PrologHelper;
import org.cs3.pl.prolog.PrologManager;
import org.cs3.pl.prolog.SourceLocation;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ST;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.StyledTextContent;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.part.ViewPart;
/**
 * @see ViewPart
 */
public class PrologConsole extends ViewPart
		implements
			IPrologListener,
			KeyListener,
			ModifyListener {
	private Hashtable messages = new Hashtable();
	AbstractList history = new ArrayList();
	int currentHistoryEntry = 0;
	boolean lastKeyCtrl = false;
	public static final String ID = "org.cs3.pl.views.PrologConsole";
	private PrologElementData[] elems;
	private int last = 0;
	/**
	 * @author windeln
	 * 
	 * To change the template for this generated type comment go to Window -
	 * Preferences - Java - Code Generation - Code and Comments
	 */
	public class ConsoleMouseListener implements MouseListener {
		public void mouseDoubleClick(MouseEvent e) {
			PrologMessage msg = (PrologMessage) messages.get(new Integer(viewer.getCaretLineNumber()
					/*.getLineAtOffset(viewer.getCaretOffset())*/));
			if (msg != null) {
				SourceLocation loc = new SourceLocation(
						msg.getReferencedLine(), msg.getReferencedFilename());
				PDTPlugin.getDefault().gotoLocation(loc);
			}
			try {
				String id = removeTrailingComma(viewer.getSelectionText());
				int intId = Integer.parseInt(id);
				PDTPlugin.getDefault().showLocationOfId(intId);
			} catch (NumberFormatException nfe) {
				// no source location found or not an ast id 
				Debug.report(nfe);
			} catch (IOException ioe) {
				Debug.report(ioe);
			} catch (PartInitException pie) {
				Debug.report(pie);
			}
		}

		/**
		 * @param string
		 * @return
		 */
		private String removeTrailingComma(String string) {
			return string.replaceAll(",", "").replaceAll(" ", "");
		}
		public void mouseDown(MouseEvent e) {
		}
		public void mouseUp(MouseEvent e) {
		}
	}
	private Text viewer;
	private StyledText query;
    private boolean ignoreOutput=false;
	private boolean inCall = false;
	private Object callLock = new Object();


	
	public PrologConsole() {
		PDTPlugin.getDefault().setPrologConsole(this);
	}
	/**
	 * @see ViewPart#createPartControl
	 */
	public void createPartControl(Composite parent) {
		SashForm composite = new SashForm(parent, SWT.VERTICAL);
		viewer = new Text(composite, SWT.BORDER | SWT.MULTI
				| SWT.H_SCROLL | SWT.V_SCROLL);
		
		//PDTPlugin.getDefault().getProject().
		viewer.setFont(JFaceResources.getTextFont());
		viewer.addMouseListener(new ConsoleMouseListener());
		viewer.addKeyListener(new KeyListener() {
			public void keyPressed(KeyEvent e) {
				try {
					if (e.character == '\r')
						e.character = '\n';
					if (e.character == 0) {
						return;
					}
					PrologManager.getInstance().getPrologWriter().write(
							e.character);
					System.out.print(e.character);
					PrologManager.getInstance().getPrologWriter().flush();
				} catch (IOException e1) {
					Debug.report(e1);
				}
			}
			public void keyReleased(KeyEvent e) {
			}
		});
		query = new StyledText(composite, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL
				| SWT.V_SCROLL);
// these keys are already bound, but do not work nevertheless
//		query.setKeyBinding(SWT.CTRL | 'c', ST.COPY); 
//		query.setKeyBinding(SWT.CTRL | 'x', ST.CUT);
//		query.setKeyBinding(SWT.CTRL | 'v', ST.PASTE);

		
		assignClipboardActions();
		
		query.setFont(JFaceResources.getTextFont());
		query.addKeyListener(this);
		int[] weights = {80, 20};
		composite.setWeights(weights);
//		final IPrologManager manager = PrologManager.getInstance().getHiddenClient();
		//final IPrologListener console = this;
		PrologManager.getInstance().addPrologListener(this);
		getPrologManagerClient();
		appendToPrologConsole(PrologManager.getInstance().getCache().getBuf());
		//		Thread thread = new Thread() {
		//			public void run() {
		//				try {
		//					//manager.init();
		//					((PrologManagerClient) manager).addPrologListener(new
		// SystemStreamsPrologListener());
		//					
		//					
		//				} catch (FileNotFoundException fnfe) {
		//					((PrologManagerClient) manager).removePrologListener(console);
		//					appendToPrologConsole("could not initialize Prolog Server: \n" +
		// fnfe.getLocalizedMessage());
		//				} catch (IOException e) {
		//					PDTPlugin.getDefault().getDisplay().asyncExec(new Runnable () {
		//						public void run() {
		//							MessageDialog.openError(PDTPlugin.getShell(), "Server
		// Initialization","Old server still running.");
		//							Runtime rt = Runtime.getRuntime();
		//							try {
		//								Process process = rt.exec("pskill");
		//								try {
		//									process.waitFor();
		//								} catch (InterruptedException e1) {
		//									// TODO Auto-generated catch block
		//									Debug.report(e1);
		//								}
		//							} catch (IOException e) {
		//								// TODO Auto-generated catch block
		//								Debug.report(e);
		//							}
		//							
		//							PDTPlugin.getDefault().getWorkbench().close();
		//						}
		//					});
		//				
		//				}
		//			}
		//		};
		//		thread.start();
	}
	/**
	 * 
	 */
	private void assignClipboardActions() {
		IActionBars bars = getViewSite().getActionBars();
		IAction action = new Action() {
			public void run() {
				if(query.isFocusControl()) {query.copy();} else {viewer.copy();}
			}
		};
		bars.setGlobalActionHandler(ActionFactory.COPY.getId(), action);
		action = new Action() {
			public void run() {
				if(query.isFocusControl()) { query.paste(); } else {viewer.paste();}
			}
		};
		bars.setGlobalActionHandler(ActionFactory.PASTE.getId(), action);
		action = new Action() {
			public void run() {
				if(query.isFocusControl()) { query.cut(); } else {viewer.cut();}
			}
		};
		ActionFactory fac = ActionFactory.CUT;
		String id = fac.getId();
		bars.setGlobalActionHandler(id, action);
		action = new Action() {
			public void run() {
				PDTPlugin.message("msg aa");
			}
		};
		//action.setActionDefinitionId("org.eclipse.pdt.ui.edit.prolog.console.completion");
		//bars.getMenuManager().add(action);
		bars.setGlobalActionHandler("org.eclipse.pdt.ui.edit.prolog.console.completion", action);
		bars.updateActionBars();
	}
	/**
	 * @see ViewPart#setFocus
	 */
	public void setFocus() {
	}
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.PrologListener#outLine(org.eclipse.swt.widgets.Event)
	 */
	public void outLine(PrologEvent e) {
		appendToPrologConsole(/* "OUT: " + */e.getData());
	}
	/*
	 * @see org.cs3.pl.PrologListener#inLine(org.eclipse.swt.widgets.Event)
	 */
	public void inLine(PrologEvent e) {
		appendToPrologConsole(/* "IN: " + */e.getData());
	}
	/*
	 * @see org.cs3.pl.PrologListener#errLine(org.eclipse.swt.widgets.Event)
	 */
	public void errLine(PrologEvent e) {
		appendToPrologConsole(/* "ERR: " + */e.getData());
	}
	/**
	 * @param object
	 */
	synchronized public void appendToPrologConsole(final Object object) {
		if (viewer == null) {
			Debug.warning("PrologConsole: viewer is null!");
			return;
		}
		if (viewer.isDisposed()) {
			//FIXME: this check is not enough! calling the widget is delayed (Display.___async___Exec,
			//			    so even if the widget is available now, it might be disposed at the time the runnable
			//			    gets executed!  Of course this check should remain here anyway.
			//System.out.println("disposed viewer: " + object);
			//FIXME: ld: this should happen earlier, at  a more apropiate point
			PrologManager.getInstance().removePrologListener(this);
			return;
		}
		try {
			getSite().getWorkbenchWindow().getWorkbench().getDisplay()
					.asyncExec(new Runnable() {
						public void run() {
							if(viewer.isDisposed()){
								Debug.warning("viewer is disposed!");
								return;
							}
							String str = object.toString();
							if (str.toUpperCase().startsWith(
									PrologMessage.ERROR))
								messages.put(new Integer(
										viewer.getLineCount() - 1),
										new PrologMessage(str, viewer
												.getLineCount(),
												PrologMessage.ERROR));
							else if (str.toUpperCase().startsWith(
									PrologMessage.WARNING))
								messages.put(new Integer(
										viewer.getLineCount() - 1),
										new PrologMessage(str, viewer
												.getLineCount(),
												PrologMessage.WARNING));
							else if (str.startsWith(PrologMessage.REFERENCE))
								messages.put(new Integer(
										viewer.getLineCount() - 1),
										new PrologMessage(str, viewer
												.getLineCount(),
												PrologMessage.REFERENCE));
							
							viewer.append(object.toString());
							
						}
					});
		} catch (Exception e) {
			System.err.println("redirected to stderr: " + object);
		}
	}
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
	 */
	public void modifyText(ModifyEvent e) {
		try {
			if (e.data != null) {
				PrologManager.getInstance().getPrologWriter().write(
						e.data.toString());
				Debug.debug("ME: " + e.data);
			}
		} catch (IOException e1) {
			Debug.report(e1);
		}
	}
	/*
	 * Handles History, polling of a running query and
	 * the polling of a debug/trace session.
	 * 
	 * There  
	 * 
	 * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
	 */
	
	
	public void keyPressed(KeyEvent e) {
		int key = e.keyCode;
		//System.out.println("key: " + key + ", " +e.stateMask +", "+ (int)'c');
		if (e.keyCode == 16777227)
			try {
				handleCompletion(e);
			} catch (IOException e1) {
				Debug.report(e1);
			}
		else
			if (e.keyCode != SWT.CTRL)
				elems = null;
		if (lastKeyCtrl) 
			handleHistory(e);
		else {
			final String text = query.getText();
			if (isInCall())  // query is running in the prolog system (native call)
				handlePolling(e);
			 else if(getPrologManagerClient().isQueryActive()) // a next() call is possible on the query 
				handleActiveQuery(e);
			else if (e.keyCode == 13 && text.length() > 2 && // a completed query was entered to the console
					 text.charAt(text.length() - 3) == '.') 
				handleNewQuery(text);
			
		}
		if (!lastKeyCtrl)
			lastKeyCtrl = (e.keyCode == 262144);
	}
	/**
	 * @param e
	 */
	private void handleHistory(KeyEvent e) {
		if (history.size() > 0)
			if (e.keyCode == ST.LINE_UP) {
				if (currentHistoryEntry < history.size())
					currentHistoryEntry++;
				setQueryTextFromHistory();
			} else if (e.keyCode == ST.LINE_DOWN) {
				if (currentHistoryEntry > 1)
					currentHistoryEntry--;
				setQueryTextFromHistory();
			}
	}
	/**
	 * @param e
	 */
	private void handleCompletion(KeyEvent e) throws IOException {
		IMetaInfoProvider metaInfo = new PrologHelper(PrologManager.getInstance().getHiddenClient());
	    if (elems != null) {
			int offset = query.getCaretOffset();
			
			int next;
			if((e.stateMask & SWT.CTRL) != 0) {
				if (last == 0)
					next = elems.length-1;
				else
					next= (last - 1) % elems.length;
			}
			else
				next= (last + 1) % elems.length;
			query.getContent().replaceTextRange(
					offset - elems[last].getLabel().length(), 
					elems[last].getLabel().length(), 
					elems[next].getLabel());
			last = next;
			setHelp(next);
		} else {
			StyledTextContent content = query.getContent();
			String text = query.getText();
			int offset = query.getCaretOffset() ;
			offset = offset == 0 ? offset : offset - 1;
			int begin = offset;
			while (PLEditor.isFunctorChar(text.charAt(begin))
					&& begin > 0)
				begin--;
			offset++;
			int len = offset - begin;
			if (!(begin == 0 && PLEditor.isFunctorChar(text.charAt(begin))))
				len--;
			String prefix = content.getTextRange(offset - len, len);
			if (prefix.length() > 0 && PLEditor.isFunctorPrefix(prefix)) {
				elems = metaInfo.getPredicatesWithPrefix(null,prefix);
				if (elems.length > 0) {
					content.replaceTextRange(offset - len, len,
							elems[0].getLabel());
					last = 0;
					setHelp(0);
				} else
					elems = null;
			}
		}
	}
	/**
	 * @param next
	 */
	private void setHelp(int next) {
		try {
			String help = elems[next].getHelp();
			if (help != null) 
				setStatusLine(help.substring(0, help.indexOf('\n')));
			else
				setStatusLine(null);
			//query.setToolTipText(help);
		} catch (IOException e1) {
			Debug.report(e1);
		}
	}
	/**
	 * @param next
	 */
	private void setStatusLine(String text) {
		getViewSite().getActionBars().getStatusLineManager().setMessage(text);
		getViewSite().getActionBars().getStatusLineManager().update(true);
	}
	/**
	 *  
	 */
	private void setQueryTextFromHistory() {
		query.setText((String) history
				.get(history.size() - currentHistoryEntry));
		query.setSelection(query.getCharCount(), query.getCharCount());
	}
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
	 */
	public void keyReleased(KeyEvent e) {
//		final String text = query.getText();
//		if (isInCall()) {
//			handlePolling(e);
//		} else if(getPrologManagerClient().isQueryActive())
//			handleActiveQuery(e);
//		else if (e.keyCode == 13 && text.length() > 2
//				&& text.charAt(text.length() - 3) == '.') {
//			handleNewQuery(text);
//		} 
//		else {
//			if(isInCall())
//				sendCharToPollingProlog(e);
//		}
		if (lastKeyCtrl && e.keyCode == 262144)
			lastKeyCtrl = false;
	}
	/**
	 * @return
	 */
	private boolean isInCall() {
		synchronized(callLock = new Object()) {
			return inCall;
		}
	}
	/**
	 * @param text
	 */
	private void handleNewQuery(final String text) {
		String historyEntry = text.substring(0, text.length() - 2);
		if (history.size() == 0 || history.size() > 0
				&& !history.get(history.size() - 1).equals(historyEntry))
			history.add(historyEntry);
		currentHistoryEntry = 1;
		viewer.append("?- " + text);
		query.setText("");
		Thread queryProcess = new Thread() {
			public void run() {
				Hashtable solution;
				solution = PrologManager.getInstance().getClient()
						.query(text.substring(0, text.length() - 3) /*+ ",flush_output"*/);
				processSolution(solution);
				
			}
		};
		queryProcess.start();
	}
	/**
	 * @param e
	 */
	private void handleActiveQuery(KeyEvent e) {
		switch (e.character) {
					case ';' :
					case 'n' :
						query.setText("");
						appendToPrologConsole(";\n");
						Thread thread = new Thread() {
							public void run() {
								processSolution(PrologManager.getInstance().getClient().next());
							}
						};
						thread.start(); //The Thread is needed, because the
						// next call may be part of
						// a tracing/debugging session. The output the console
						// must
						// wait for the calling Runnable to finish -> deadlock
						// in the Eclipse Display Scheduler
						break;
					case 'a' :
					case '\r' :
					case '\n' :
					case ' ' :
						getPrologManagerClient().abort();
						query.setText("");
						break;
					default:
						query.setText("");
				}
	}
	/**
	 * @param e
	 */
	private void handlePolling(KeyEvent e) {
		sendCharToPollingProlog(e);
		appendToPrologConsole("\n");
		e.doit = false;
	}
	/**
	 * @return
	 */
	private boolean queryAborted() {
		return false;
	}
	/**
	 * @param e
	 */
	private void sendCharToPollingProlog(KeyEvent e) {
		sendCharToPollingProlog(e, true);
	}
	private void sendCharToPollingProlog(KeyEvent e, boolean appendLineFeed) {
		query.setText("");
		//appendToEditor(" a \n\nabort\n");
		String str;
		if (appendLineFeed)
			str = e.character + "\n";
		else
			str = "" + e.character;
		try {
			PrologManager manager =  PrologManager.getInstance();
			manager.getPrologWriter().write(str);
			manager.getPrologWriter().flush();
		} catch (IOException e1) {
			Debug.report(e1);
		}
		System.out.print(e.character);
	}
	/**
	 *  
	 */
	private boolean processSolution(Hashtable solution) {
		//Hashtable solution =PrologManagerClient.getInstance().next();
		if (solution != null) {
			Enumeration keys = solution.keys();
			for (; keys.hasMoreElements();) {
				String varname = (String) keys.nextElement();
				Object value = solution.get(varname);
				String strValue;
				if(value instanceof String) 
					strValue = (String)value;
				else if(value instanceof Object[])
					value =  objectArrayToListString((Object[])value);
				appendToPrologConsole("\n " + varname + " = " + value + " ");
			}
			return true;
		}
		return false;
	}
	
	private String objectArrayToListString(Object[] value){
		String result = "[";
		for (int i = 0; i < value.length; i++) {
			if(i > 0)
				result += ",";
			if (value[i] instanceof String) {
				result += value[i];
			} else if (value[i] instanceof Object[]) {
				result += objectArrayToListString((Object[])value[i]);
			} else {
				String msg = "Unknown Class Type class: " +value[i].getClass().getName() + "(" + value[i] + ")";
				Debug.error(msg);
				result += (msg);
			}

		}
		return result + "]";
		
	}
	
	public void reset() {
		viewer.setText("");
	}

// disabled for now. 
//	private boolean isPolling() {
//		return getPrologManagerClient().isProcessing();
//	}
	/**
	 * @return
	 */
	private IPrologClient getPrologManagerClient() {
		return PrologManager.getInstance().getClient();
	}
	/**
	 * @return
	 */
	public StyledText getQueryWidget() {
		return query;
	}
   
	
	public void enterCatchedCall(PrologEvent e) {
	    synchronized(callLock) {
		    ignoreOutput=e.getClient().isHidden();
		    inCall = true;
	    }
    }
    
	public void exitCatchedCall(PrologEvent e) {
	    synchronized(callLock) {
	        ignoreOutput=false;
	        inCall = false;
	    }
    }
   
	public void newDataAvailable(PrologEvent e) {
	    synchronized(callLock) {
	    	if(ignoreOutput)return;
	    }
        switch(e.getStream()){
        	case PrologManager.OUT_STREAM:
        	    outLine(e);
        	break;
        	case PrologManager.ERR_STREAM:
        	    errLine(e);
        	break;
        	case PrologManager.IN_STREAM:
        	    inLine(e);
        	break;
        	default:
        	    throw new IllegalArgumentException("bad stream identifier.");
        }
        
    }
}