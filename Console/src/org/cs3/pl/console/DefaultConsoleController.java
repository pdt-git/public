/*
 */

package org.cs3.pl.console;

import org.cs3.pl.common.Debug;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;



/**
 */
public class DefaultConsoleController implements ConsoleController,
        ConsoleModelListener {

    protected boolean thatWasMe = false;

    protected ConsoleHistory history = new DefaultConsoleHistory();
    
    protected ConsoleCompletionProvider completionProvider = null;
	
    /**
	 * @return Returns the history.
	 */
	public ConsoleHistory getHistory() {
		return history;
	}
	/**
	 * @param history The history to set.
	 */
	public void setHistory(ConsoleHistory history) {
		this.history = history;
		if(history!=null){
			history.setConsoleModel(model);
		}
	}
    protected ConsoleModel model;

    protected ConsoleUI ui;

    

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleController#keyStrokeIntercepted(int)
     */
    public boolean keyStrokeIntercepted(int keyCode, char keyChar) {
        if (model == null) {
            return false;
        }
        if (model.isSingleCharMode()) {            
            return false;
        } else {
            switch (keyCode) {
            case SWT.CR:
            case SWT.KEYPAD_CR:                
                return true;

            case SWT.ARROW_UP:
            case SWT.KEYPAD_8:
                return true;

            case SWT.ARROW_DOWN:
            case SWT.KEYPAD_2:
                return true;
            
            case SWT.TAB:            
            	doCompletion();        	
            	return false;
            default:
                return true;
            }
        }
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.views.ConsoleController#keyPressed(int, char)
     */
    public void keyPressed(int keyCode, char keyChar) {
        if (model == null) {
            return ;
        }
        if (model.isSingleCharMode()) {
            model.putSingleChar(keyChar);           
        } else {
            switch (keyCode) {
            case SWT.CR:
            case SWT.KEYPAD_CR:
                model.commitLineBuffer();
                break;
            case SWT.ARROW_UP:
            case SWT.KEYPAD_8:
            	Debug.debug("UP");
                history.previous();
            	break;
            case SWT.ARROW_DOWN:
            case SWT.KEYPAD_2:
            	history.next();
                break;
            case SWT.TAB:
            	break;
            default:
                break;
            }
        }
        
    }

    
    /**
	 * 
	 */
	private void doCompletion() {
	    if(completionProvider==null){
			return;
		}
		
		final int caretPosition = ui.getCaretPosition();
		Runnable work = new Runnable(){
            public void run() {
                
                final CompoletionResult r = completionProvider.doCompletion(model.getLineBuffer(),caretPosition);
                final Runnable notify = new Runnable(){
        		    public void run(){
        		        completionAvailable(r);
        		    }
        		};
                ui.getDisplay().asyncExec(notify);
            }
        };
        
        new Thread(work,"Console Completion Worker").start();
        
		}
	
	private void completionAvailable(CompoletionResult r){
	   
	    if( ! model.getLineBuffer().equals(r.getOriginalLineContent())){
	        Debug.debug("completion discarded.");
	        return;
	    }
	    if( ui.getCaretPosition()!=r.getOriginalCaretPosition()){
	        Debug.debug("completion discarded.");
	        return;
	    }
	    
	    String[] options = r.getOptions();
		Debug.debug("found "+options.length+" completions");
		model.setLineBuffer(r.getNewLineContent());
		ui.setCaretPosition(r.getNewCaretPosition());
		if (options.length>1){
		    StringBuffer buf = new StringBuffer();
			buf.append("\n");
			for(int i=0;i<options.length;i++){
				buf.append(options[i]);
				buf.append("\n");
			}			
			ui.appendOutput(buf.toString());
		}
	
	}
	/*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleController#inputModificationIntercepted(java.lang.String)
     */
    public boolean inputModificationIntercepted(String old, int from, int to, String text) {
        if (model == null) {
            return false;
        }        
        return true;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.views.ConsoleController#inputModified(java.lang.String)
     */
    public void inputModified(String newInput) {
        if(! thatWasMe){
            thatWasMe=true;
            model.setLineBuffer(newInput);
            thatWasMe=false;
        }
    }

    
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleController#setUI(org.cs3.pl.views.ConsoleUI)
     */
    public void setUI(ConsoleUI c) {
        if (this.ui != c) {
        	this.ui = c;        	
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleController#setModel(org.cs3.pl.views.ConsoleModel)
     */
    public void setModel(ConsoleModel consoleModel) {
        if (model == consoleModel) {
            return;
        }
        if (model != null) {
            model.removeConsoleListener(this);
        }
        this.model = consoleModel;
        if (model != null) {
            model.addConsoleListener(this);
        }
        
        if(history!=null){
        	history.setConsoleModel(model);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleModelListener#onOutput(org.cs3.pl.views.ConsoleModelEvent)
     */
    public void onOutput(final ConsoleModelEvent e) {
        if(ui==null){
        	return;
        }
    	Display display = ui.getDisplay();
        if (Display.getCurrent() != display) {
            display.asyncExec(new Runnable() {
                public void run() {
                    onOutput(e);
                }
            });
        } else {
            ui.appendOutput(e.getOutput());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleModelListener#onEditBufferChanged(org.cs3.pl.views.ConsoleModelEvent)
     */
    public void onEditBufferChanged(final ConsoleModelEvent e) {
        if(ui==null){
        	Debug.warning("no UI, dropping EditBufferChange: "+e.getNewLineState());
        	return;
        }
    	Display display = ui.getDisplay();
        if(display==null){
        	Debug.warning("UI seems to be unavailable. dropping EditBufferChange: "+e.getNewLineState());
        	return;
        }
        if (Display.getCurrent() != display) {
            display.asyncExec(new Runnable() {
                public void run() {
                    onEditBufferChanged(e);
                }
            });
        } else if(! thatWasMe){
            thatWasMe = true;
            String text = e.getNewLineState();
            ui.setLineBuffer(text==null ? "" : text);
            thatWasMe = false;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleModelListener#onCommit(org.cs3.pl.views.ConsoleModelEvent)
     */
    public void onCommit(final ConsoleModelEvent e) {
        Display display = ui.getDisplay();
        if (Display.getCurrent() != display) {
            display.asyncExec(new Runnable() {
                public void run() {
                    onCommit(e);
                }
            });
        } else {
            thatWasMe = true;
            ui.setLineBuffer("");
            thatWasMe = false;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleModelListener#onModeChange(org.cs3.pl.views.ConsoleModelEvent)
     */
    public void onModeChange(ConsoleModelEvent e) {
    	//pfff...
    }

    
    
	/**
	 * @return Returns the completionProvider.
	 */
	public ConsoleCompletionProvider getCompletionProvider() {
		return completionProvider;
	}
	/**
	 * @param completionProvider The completionProvider to set.
	 */
	public void setCompletionProvider(
			ConsoleCompletionProvider completionProvider) {
		this.completionProvider = completionProvider;
	}
	/* (non-Javadoc)
	 * @see org.cs3.pl.console.ConsoleModelListener#afterConnect(org.cs3.pl.console.ConsoleModelEvent)
	 */
	public void afterConnect(ConsoleModelEvent e) {
		// TODO should we handle this?
		
	}
	/* (non-Javadoc)
	 * @see org.cs3.pl.console.ConsoleModelListener#beforeDisconnect(org.cs3.pl.console.ConsoleModelEvent)
	 */
	public void beforeDisconnect(ConsoleModelEvent e) {
		// TODO should we handle this?		
	}
}