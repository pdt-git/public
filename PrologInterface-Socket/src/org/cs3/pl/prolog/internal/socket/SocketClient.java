/*
 */
package org.cs3.pl.prolog.internal.socket;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.UnknownHostException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.LogBuffer;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.internal.ReusablePool;

/**
 */
public class SocketClient  {
    
    public static final String GIVE_TERM = "GIVE_TERM";
    public static final String END_OF_SOLUTION = "END_OF_SOLUTION";
    public static final String MORE = "MORE?";
    public static final String SHUTDOWN = "SHUTDOWN";
    public static final String BYE = "BYE";
    
    private class InputStreamProxy extends InputStream{
        InputStream in;
        private LogBuffer logBuf;
        /**
         * @param in
         */
        public InputStreamProxy(InputStream in,LogBuffer logBuf) {
            super();
            this.in = in;
            this.logBuf=logBuf;
        }
        public int available() throws IOException {
            lock();
            try{
                return in.available();
            }finally{
                unlock();
            }
        }        
        
        public void close() throws IOException {
            lock();
            try{
                SocketClient.this.close();
            }finally{
                unlock();
            }
        }
        public synchronized void mark(int readlimit) {
            lock();
            try{
                in.mark(readlimit);
            }finally{
                unlock();
            }
        }
        public boolean markSupported() {
            lock();
            try{
                return in.markSupported();
            }finally{
                unlock();
            }
        }
        /* (non-Javadoc)
         * @see java.io.InputStream#read()
         */
        public int read() throws IOException {
            lock();
            try{
                
                int read = in.read();
                logBuf.log("read",(char)read);
                return read;
            }finally{
                unlock();
            }
        }
        public int read(byte[] b) throws IOException {
            lock();
            try{
                int read = in.read(b);
                logBuf.log("read",b,0,read);
                return read;
            }finally{
                unlock();
            }
        }
        public int read(byte[] b, int off, int len) throws IOException {
            lock();
            try{
               int read = in.read(b,off,len);
               logBuf.log("read",b,off,read);
            return read;
            }finally{
                unlock();
            }
        }
        public synchronized void reset() throws IOException {
            lock();
            try{
                in.reset();
            }finally{
                unlock();
            }
        }
        public long skip(long n) throws IOException {
            lock();
            try{
                return in.skip(n);
            }finally{
                unlock();
            }
        }
        
    }
    private class OutputStreamProxy extends OutputStream{
        OutputStream out;
        private LogBuffer logBuf;
        /**
         * @param out
         */
        public OutputStreamProxy(OutputStream out,LogBuffer logBuf) {
            super();
            this.out = out;
            this.logBuf=logBuf;
        }
        public void close() throws IOException {
            lock();
            try{
                SocketClient.this.close();
            }finally{
                unlock();
            }
        }
        public void flush() throws IOException {
            lock();
            try{
                out.flush();
            }finally{
                unlock();
            }
        }
        public void write(byte[] b) throws IOException {
            lock();
            try{
                out.write(b);
                logBuf.log("write",b);
            }finally{
                unlock();
            }
        }
        public void write(byte[] b, int off, int len) throws IOException {
            lock();
            try{
                out.write(b,off,len);
                logBuf.log("write",b,off,len);
            }finally{
                unlock();
            }
        }
        public void write(int b) throws IOException {
            lock();
            try{
                out.write(b);
                logBuf.log("write",(char)b);
            }finally{
                unlock();
            }
        }
}

    public static final String GIVE_COMMAND = "GIVE_COMMAND";
    public static final String GIVE_SYMBOL = "GIVE_SYMBOL";
    public static final String GIVE_PREFIX = "GIVE_PREFIX";
    public static final String CONSULT = "CONSULT";
    public static final String UNCONSULT = "UNCONSULT";
    public static final String LIST = "LIST";
    public static final String QUERY = "QUERY";
    public static final String QUERY_ALL = "QUERY_ALL";
    public static final String IS_CONSULTED = "IS_CONSULTED";    
    public static final String GO_AHEAD = "GO_AHEAD";    
    public static final String LINE_SEPARATOR = "\n";
    public static final String OK = "OK";
    public static final String ERROR = "ERROR: ";
    public final static String YES = "YES";
    public final static String NO = "NO";
    public final static String EOF = "end_of_file"; 
    public static final String USING_SYMBOL = "USING_SYMBOL: ";
    public static final String PING = "PING";
    public static final String PONG = "PONG";
   
    private int lockCounter=0;
    private Thread owner=null;
    private Object ownerLock = new Object();
    private BufferedReader reader;

    private ReusableSocket socket;
    private ReusablePool pool;
    private BufferedWriter writer;
    private static final long TIMEOUT = 1000;
    private static final String RESET = "RESET";
    
    
    /**
     * @param socket
     * @throws IOException
     */
    public SocketClient(ReusableSocket socket) throws IOException {
        super();
        this.socket = socket;
        reader = new BufferedReader(new InputStreamReader(getInputStream()));
        writer = new BufferedWriter(new PrintWriter(getOutputStream()));
        reset();
    }
    /**
     * @param string
     * @param port
     * @throws IOException
     * @throws UnknownHostException
     */
    public SocketClient(String string, int port) throws UnknownHostException, IOException {
        this(new ReusableSocket(string,port));
    }
    /**
     * @throws IOException
     * 
     */
    public void close() throws IOException {
        if (socket==null){
            return;
        }
        lock();
        reset();
       try{
           //if(false){
           if(pool!=null){
               pool.recycle(socket);
           }
           else {
               socket.destroy();
           }
       }
        finally{            
            socket=null;
            unlock();            
        }
    }

  
    
/**
 * @throws IOException
 * @throws PrologException
     * 
     */
    public void reset() throws PrologException, IOException {
        lock();
        try{
            while(reader.ready()){
            	reader.read();
            }
            writeln(EOF+".");
            readUntil(OK);
        }
        finally{
            unlock();
        }
        
    }
//    /**
//     * @return
//     */
//    public boolean ping() {
//       lock();
//       try{
//           while(reader.ready()){
//               reader.read();
//           }
//           writeln(PING);
//           readUntil(PONG);
//           return false;
//       } catch (IOException e) {
//           Debug.report(e);
//           return false;
//    }
//       finally{
//           unlock();
//       }
//    }
    public InputStream getInputStream() throws IOException{
        if (socket==null){
            throw new IllegalStateException("Socket is closed, go away. ");
        }
        return new InputStreamProxy(socket.getInputStream(),socket.getLogBuffer());
    }
    
    public OutputStream getOutputStream() throws IOException{
        if (socket==null){
            throw new IllegalStateException("Socket is closed, go away. ");
        }
        return new OutputStreamProxy(socket.getOutputStream(),socket.getLogBuffer());
    }
    /**
     * 
     */
    public void lock() {
        if (socket==null){
            throw new IllegalStateException("Socket is closed, go away. ");
        }
        synchronized(ownerLock){
            Thread current = Thread.currentThread();            
//            while(current!=owner&&lockCounter>0){
//                try {
//                    ownerLock.wait(5000);
//                } catch (InterruptedException e) {
//                    Debug.report(e);
//                }
//            }
            if(current!=owner&&lockCounter>0){
                	throw new IllegalThreadStateException("The client socket is locked by another thread: "+owner.getName());
            }
            owner=current;
            lockCounter++;
        }
        
    }

    
    
    public String readln()throws IOException{
        return reader.readLine();
    }
    
    /**
     * same as readUntil(prefix,null).
     * @param prefix
     * @return
     * @throws PrologException
     * @throws IOException
     */
    public  String readUntil(String prefix) throws PrologException, IOException{
        return readUntil(prefix,null);
    }


    /**
     * reads until a line with the given prefix is recieved. This call will
     * block until line is read that is that starts with the given prefix or the
     * special prefix <cpde>ERROR</code>, or if <code>EndOfStream</code> is
     * reached. In the two latter cases, a <code>ConsultException</code> is
     * raised. In all three cases only complete lines (i.e. terminated with a
     * newline char) are read.
     * 
     * @param prefix
     *                    The awaited prefix
     * @param data
     *                    all data recieved BEFORE the above prefix is appended to this
     *                    StringBuffer. If this argument is <code>null</code>, .the data is 
     *                    silently discarded.
     * @return the remaining postfix of the line beginning with prefix.
     * @throws IOException
     *                    you never know...
     * @throws PrologException
     *                    if a line starting with <code>ERROR</code> or <code>
     *                    EndOfStream</code> is recieved.
     */
    public String readUntil(String prefix, StringBuffer data)
            throws IOException, PrologException {
        if (socket==null){
            throw new IllegalStateException("Socket is closed, go away. ");
        }
        String string = "";
        while (!string.startsWith(prefix)) {
            if(data!=null){
                data.append(string);
            }
            string = reader.readLine();
            if (string == null) {                
                Debug.warning("there was an error. Exceptions will be thrown.");
                File logFile = Util.getLogFile("fail.log");
                
                PrintStream p = new PrintStream(new BufferedOutputStream(new FileOutputStream(logFile)));
                socket.getLogBuffer().printLog(p);
                p.close();
                Debug.warning("a connection log was saved to :"+logFile.getCanonicalPath());
                throw new PrologException(
                        "EndOfStream read while waiting for " + prefix);
            }
            Debug.debug("read: " + string);
            if (string.startsWith(SocketClient.ERROR)) {
                throw new PrologException(
                        "Peer reported an error while waiting for " + prefix
                                + ": " + string.substring(SocketClient.ERROR.length()));
            }
        }
        return string.substring(prefix.length());
    }
    
    public void unlock() {
        synchronized(ownerLock){
            Thread current = Thread.currentThread();
            if(current!=owner){
                throw new IllegalThreadStateException("current thread is not the owner!");
            }
            if(lockCounter<=0){
                throw new IllegalStateException("Wer hat in mein Töpfchen geschissen?!");
            }
            lockCounter--;
            if(lockCounter==0){
                owner = null;
                ownerLock.notifyAll();
            }
        }
    }
    public void writeln(String line)throws IOException{
        if (socket==null){
            throw new IllegalStateException("Socket is closed, go away. ");
        }
        writer.write(line+LINE_SEPARATOR);
        writer.flush();
    }
 
    public ReusablePool getPool() {        
        return pool;
    }
    public void setPool(ReusablePool pool) {
        this.pool = pool;
    }
}
