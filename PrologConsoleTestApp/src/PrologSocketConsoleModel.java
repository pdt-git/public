
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.util.HashSet;
import java.util.Iterator;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.console.ConsoleModel;
import org.cs3.pl.console.ConsoleModelEvent;
import org.cs3.pl.console.ConsoleModelListener;

/**
 * @author terra
 */
public class PrologSocketConsoleModel implements ConsoleModel {
	
        
    private class ConsoleReader implements Runnable {
        
        private BufferedReader reader;

        public ConsoleReader(BufferedReader reader){
            this.reader = reader;
        }

        public void run() { 
            char[] buf = new char[255];
            while (true){
                try {
                    int count = reader.read(buf);
                    if(count <0){
                        throw new IOException("EndOfStream read.");
                    }
                    String data = String.copyValueOf(buf,0,count);
                    //Debug.debug("CONSOLE RECIEVED: " + data);
                                        
                    if (data == null){
                        throw new IOException("readLine() returned null");
                    }
                    
                    ConsoleModelEvent cme = new ConsoleModelEvent(PrologSocketConsoleModel.this,data,false);
                    
                    
                    fireOutputEvent(cme);
                    
                } catch (IOException e) {
                    Debug.report(e);
                    disconnect();
                    break;
                }
            }
            
            Debug.info("Console Reader Thread terminating");
        }
        
        
    }
    
    private int port;
    
    private Thread readerThread = null;    
    
    private boolean singleCharMode = false;
    
    private String lineBuffer = "";
    private BufferedWriter writer;

    private HashSet listeners = new HashSet();

    private Socket socket;

    public PrologSocketConsoleModel()  {      
        this.port = 5567;
        connect();
    }
    public PrologSocketConsoleModel(boolean doConnect)  {      
        this.port = 5567;
        if(doConnect){
        	connect();
        }
    }

    public String getLineBuffer() {
        return lineBuffer;
    }

    public void setLineBuffer(String buffer) {
        String oldBuffer=lineBuffer;
    	lineBuffer = buffer;
        fireEditBufferChangedEvent(oldBuffer, lineBuffer);
    }

	synchronized public void commitLineBuffer() {
        if (singleCharMode)
            throw new IllegalStateException("In single char mode");
        
        
        if (!isConnected()) {
			connect();
		}

        ConsoleModelEvent cme = new ConsoleModelEvent(PrologSocketConsoleModel.this,lineBuffer+"\n",false);
        
        fireOutputEvent(cme);
        
		try {
			writer.write(lineBuffer);
			writer.write("\n");
			writer.flush();

			cme = new ConsoleModelEvent(this,lineBuffer);			

			fireCommitEvent(cme);
		} catch (IOException e) {
			disconnect();
		}        
    }


    public void addConsoleListener(ConsoleModelListener cml) {
        synchronized (listeners){
            listeners.add(cml);
        }
    }
    
    public void removeConsoleListener(ConsoleModelListener cml){
        synchronized (listeners){
            listeners.remove(cml);
        }
    }

    public void putSingleChar(char c) {
        if (!singleCharMode)
            throw new IllegalStateException("In line mode");
        
        synchronized (this){
            if (!isConnected()){
                try {
                    wait(500);
                    connect();
                } catch (InterruptedException e1) {
                    return;
                }
            }
        
        
        try {
                writer.write(c + "\n");
                writer.flush();
            } catch (IOException e) {
                disconnect();
            }
        }
    }


    public boolean isSingleCharMode() {
        return singleCharMode;
    }
    
    public synchronized void connect(){
    	if(isConnected()){
    		Debug.warning("Seems we are already connected?");
    		return;
    	}
    	//knock, knock...
    	Debug.info("probing port "+port);
    	if(!isServerActive()){
    		Debug.info("No one is listening on port "+port+", i give up for now.");
    		return;
    	}
    	try {
    		Debug.info("connecting console to server at port "+port);
        	socket = new Socket("localhost", port);
            writer = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
            BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            readerThread = new Thread(new ConsoleReader(reader));
            readerThread.setDaemon(true);
            readerThread.setName("Console Reader Thread");
            readerThread.start();
                
            
            
            Debug.debug("Connect complete");
        } catch (IOException e) {
            Debug.report(e);
            Debug.error("Connect failed, trying to disconnect gracefully...");
            disconnect();
        }
    }
    
    
/* (non-Javadoc)
 * @see java.lang.Object#finalize()
 */
    protected void finalize() throws Throwable {    
        super.finalize();
        disconnect();
    }
    public synchronized void disconnect() {
    	Debug.debug("Disconnect began");
        synchronized (this){
            try {
                if (socket != null){
                	writer.write("end_of_file.\n");
                	writer.flush();
                    socket.close();
                }
            } catch (IOException e) {
                Debug.report(e);
            }
            
            socket = null;
            writer = null;
            readerThread = null;            
        }
        
        ConsoleModelEvent cme = new ConsoleModelEvent(this,"<<< (Not an) ERROR: Connection to Prolog Process closed >>>",true);

        
        fireOutputEvent(cme);
        
        Debug.debug("Disconnect complete");
    }
    

    void fireOutputEvent(ConsoleModelEvent cme) {
        HashSet l;
        
        synchronized (listeners){
            l = (HashSet) listeners.clone();
        }
        
        for (Iterator i = l.iterator(); i.hasNext(); ){
            ConsoleModelListener list = (ConsoleModelListener) i.next();
            list.onOutput(cme);
        }
        
    }
    
    void fireModeChange(ConsoleModelEvent cme) {
        HashSet l;
                
        synchronized (listeners){
            l = (HashSet) listeners.clone();
        }
        
        for (Iterator i = l.iterator(); i.hasNext(); ){
            ConsoleModelListener list = (ConsoleModelListener) i.next();
            list.onModeChange(cme);
        }
        
    }
    
    private void fireCommitEvent(ConsoleModelEvent cme) {
        HashSet l;

        synchronized (listeners) {
            l = (HashSet) listeners.clone();
        }

        for (Iterator i = l.iterator(); i.hasNext();) {
            ConsoleModelListener list = (ConsoleModelListener) i.next();
            list.onCommit(cme);
        }  
    }
    
    private void fireEditBufferChangedEvent(String oldBuffer,String buffer) {
    	ConsoleModelEvent ev = new ConsoleModelEvent(this,oldBuffer,buffer);
    	
        HashSet l;

        synchronized (listeners) {
            l = (HashSet) listeners.clone();
        }		

        for (Iterator i = l.iterator(); i.hasNext();) {
            ConsoleModelListener list = (ConsoleModelListener) i.next();
            list.onEditBufferChanged(ev);
        }  
	}
	

    

	


	private boolean isServerActive() {
		return Util.probePort(port,"end_of_file.\n");
	}


	


	public  boolean isConnected() {
		if(socket==null){
			return false;
		}
		return socket.isConnected();
	}


	
	/**
	 * @return Returns the port.
	 */
	public int getPort() {
		return port;
	}
	/**
	 * @param port The port to set.
	 */
	public void setPort(int port) {
		this.port = port;
	}
}
