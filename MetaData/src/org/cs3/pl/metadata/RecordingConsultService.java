package org.cs3.pl.metadata;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.net.Socket;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.IPrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.SessionException;

/**
 * A consult service that keeps copies of the consulted data in the local
 * filesystem.
 * <p>
 * It is assumed that the used symbols have a path-like structure, using a slash
 * character("/") as delimiter. The logical filenames used within the prolog
 * system are constructed from the symbols by prepending an optional prefix
 * (property <code>prefix</code>).
 * <p>
 * <b>It is assumed that the resulting string forms a POSIX-complient, absolute
 * file system path. <b>In Particular, both parts should start but not end with
 * a "/". Some checking and corrections does take place, but it's basicaly up to
 * the using code to make sure, that the supplied path actualy "makes sense".
 * <p>
 * 
 * The OS filesystem paths to the files for saving the records are constructed
 * from the symbolic paths by replacing all "/" characters with the file
 * delimiter character used on the host platform.
 *  
 */
public class RecordingConsultService implements ConsultService {

    /*
     * dispatches data written to the stream to two other streams, referenced as
     * "sink" and "record". The streams are treated differently during close:
     * the record stream is simply closed. the sink is not closed, but instead,
     * EOF char is written to it.
     * 
     * This class does no buffering itself. It is recommended to use buffered
     * streams as backends.
     */
    private class _OutputStream extends OutputStream {

        private static final String EOF = "end_of_file.\n";

        OutputStream record;

        OutputStream sink;

        String unprefixedSymbol;


        public void close() throws IOException {
            writer.write("\n"+EOF);
            writer.flush();
            if(record!=null){
                record.close();
            }
            String string = "";
            while (!string.equals(OK)) {
                string = reader.readLine();
                Debug.debug("read: " + string);
            }
            fireConsultDataChanged(new ConsultServiceEvent(RecordingConsultService.this,unprefixedSymbol));
        }

        public void flush() throws IOException {
            sink.flush();
            if(record!=null){
                record.flush();
            }
        }

        public void write(byte[] b) throws IOException {
            Debug.debug("writing: '" + new String(b) + "'");
            sink.write(b);
            if(record!=null){
                record.write(b);
            }
        }

        public void write(byte[] b, int off, int len) throws IOException {
            Debug.debug("writing: '" + new String(b, off, len) + "'");
            sink.write(b, off, len);
            if(record!=null){
                record.write(b, off, len);
            }
        }

        public void write(int b) throws IOException {
            Debug.debug("writing: '" + (char) b + "'");
            sink.write(b);
            if(record!=null){
                record.write(b);
            }
        }
    }

    private static final String GIVE_SYMBOL = "GIVE_SYMBOL";

    private static final String GO_AHEAD = "GO_AHEAD";

    private static final String LINE_SEPARATOR = "\n";

    private static final String USING_SYMBOL = "USING_SYMBOL";
    private static final Object OK = "OK";

    private int port = 5624;

    private String prefix = "";

    private String extension = ".pl";
    private IPrologInterface prologInterface = null;

    private BufferedReader reader = null;

    private Socket socket = null;

    private BufferedWriter writer = null;

    private Vector listeners=new Vector();
    
    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public String getPrefix() {
        return prefix;
    }

    public void setPrefix(String prefix) {
        //strip trailing file separator if necessary
        //ld: NOTE: using forward slashes here is NOT an accident.
        //This is NOT a host filesystem path.
        if (prefix.endsWith("/")) {
            prefix = prefix.substring(0, prefix.length() - 1);
        }
        if (!prefix.startsWith("/")) {
            prefix = "/" + prefix;
        }
        this.prefix = prefix;
    }

    public IPrologInterface getPrologInterface() {
        return prologInterface;
    }

    public void setPrologInterface(IPrologInterface prologInterface) {
        this.prologInterface = prologInterface;
    }

    public void connect() throws IOException {
        socket = new Socket("localhost", port);
        reader = new BufferedReader(new InputStreamReader(socket
                .getInputStream()));
        writer = new BufferedWriter(new OutputStreamWriter(socket
                .getOutputStream()));

    }

    public void disconnect() {
        try {
            socket.close();
        } catch (IOException e) {
            Debug.report(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.metadata.ConsultService#getOutputStream()
     */
    public PrintStream getOutputStream(String s) {
        return getOutputStream_impl(s,true);
    }
    
    protected PrintStream getOutputStream_impl(String s, boolean keepRecord) {
        String symbol = getPrefixedSymbol(s);
        
        if (prologInterface == null) {
            Debug.warning("Cannot remove zombie mark from symbol " + s
                    + " since i do not have a living Prolog Interface.");            
        }
        else{
            try {
                PrologSession session = prologInterface.getSession();
                session.query("undelete_symbol('"+symbol+"')");
                session.dispose();
            } catch (SessionException e1) {
                Debug.report(e1);
            }
        }
        Debug.debug("prefix: " + prefix + ", s: " + s);
        String string = "";
        try {
            while (!string.equals(GIVE_SYMBOL)) {
                string = reader.readLine();
                Debug.debug("read: " + string);
            }
            writer.write(symbol + LINE_SEPARATOR);
            writer.flush();
            string = "";
            while (!string.equals(GO_AHEAD)) {
                string = reader.readLine();
                Debug.debug("read: " + string);
            }
            _OutputStream stream = new _OutputStream();
            stream.sink = socket.getOutputStream();
            if(keepRecord){
                String filename =getFileName(s);
                File file = new File(filename);
                Debug.debug("record filename:" + filename);
                if (!file.exists()) {
                    file.getParentFile().mkdirs();
                    file.createNewFile();
                }
                stream.record = new BufferedOutputStream(new FileOutputStream(file));
            }           
           
            stream.unprefixedSymbol=s;
            return new PrintStream(stream);
        } catch (IOException e) {
            Debug.report(e);
            return null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.metadata.ConsultService#isConsulted(java.lang.String)
     */
    public boolean isConsulted(String s) {
        //ld: NOTE: using forward slashes here is NOT an accident.
        //This is NOT a host filesystem path.
        String symbol = getPrefixedSymbol(s);

        if (prologInterface == null) {
            Debug
                    .warning("cannot determine if '"
                            + s
                            + "' i consulted, since i have no PrologInterface. I will return false.");
            return false;
        }
        String query = "consulted_symbol('" + symbol + "')";
        Hashtable r = null;
        try {
            PrologSession session = prologInterface.getSession();
            r = session.query(query);
            session.dispose();
        } catch (SessionException e) {
            Debug.report(e);
        }
        return r != null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.metadata.ConsultService#unconssult(java.lang.String)
     */
    public void unconsult(String s) {
        if (!isConsulted(s)) {
            return;
        }
        if (prologInterface == null) {
            Debug.error("cannot unconsult without living prolog interface!");
            return;
        }
        //consult an empty "file" for the given symbol.
        getOutputStream(s).close();
        String symbol =getPrefixedSymbol(s);
        try {
            PrologSession session = prologInterface.getSession();
            session.query("delete_symbol('"+symbol+"')");
            session.dispose();
        } catch (SessionException e) {
            Debug.report(e);
        }
        String filename = getFileName(s);
        File file= new File(filename);
        if(file.exists()){
            file.delete();            
        }
        fireConsultDataChanged(new ConsultServiceEvent(this,s));
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.metadata.ConsultService#getTimeStamp(java.lang.String)
     */
    public long getTimeStamp(String s) {
        if(!isConsulted(s)){
            return -1;
        }
        String filename = getFileName(s);
        File file= new File(filename);
        if(file.exists()){
            long t = file.lastModified();
            return t==0? -1: t;
        }        
        return -1;
    }
    
    /**
     * creates an absolute os filesystem path from an unprefixed  symbol
     * @param s
     * @return
     */
    private String getFileName(String s) {
        String symbol = getPrefixedSymbol(s);        
        String filename = symbol.replaceAll("/", File.separator);
        return filename;
    }

    

    public void reload(){
        reload(new File(getFileName("")));        
        fireConsultDataChanged(new ConsultServiceEvent(this));
    }
    
    /**
     * creates a prefixed symbol  from an unprefixed  symbol
     * 
     * @param s
     * @return
     */
    private String getPrefixedSymbol(String s) {
        String symbol = s.startsWith("/") ? prefix + s : prefix + "/" + s;
        return symbol;
    }
    
    private String getUnPrefixedSymbol(String prefixedSymbol ){
        return prefixedSymbol.substring(prefix.length());
    }
    
    private String getUnPrefixedSymbol(File f ){
        return getUnPrefixedSymbol(getPrefixedSymbol(f));
    }
	/**
	 * creates a prefixed symbol from a file.
	 *  
	 * @param 
	 * @return
	 */
    private String getPrefixedSymbol(File f){
        try {
            String symbol = f.toURI().toURL().getFile();
            if(! symbol.startsWith(prefix)){
                throw new IllegalArgumentException("the given file is not within my domain, sorry.");
            }
            return symbol;
        } catch (IOException e) {
            Debug.report(e);
            return null;
        }
               
    }
    
    private void reload(File f){
        Debug.debug("reload(File) visiting: "+f.getAbsolutePath());
        if(!f.exists()){
            Debug.debug("\t --> dos not exist: "+f.getAbsolutePath());
            return;
        }
        
        if(f.isFile()){
            Debug.debug("\t --> is a file: "+f.getAbsolutePath());
            if(! f.getName().endsWith(getExtension())){
                Debug.debug("\t --> wrong extension: "+f.getAbsolutePath());
                return;
            }
            BufferedInputStream fr=null;
            PrintStream out=null;
            try {
                Debug.debug("\t --> loading: "+f.getAbsolutePath());
                 fr = new BufferedInputStream(new FileInputStream(f));
                out = getOutputStream_impl(getUnPrefixedSymbol(f),false);
                byte[] buf=new byte[255];
                int read=-1;
                while((read=fr.read(buf))>-1){
                    out.write(buf,0,read);
                }
            } catch (Throwable e) {
             
                Debug.report(e);
            }
            finally{
                out.close();
                try {
                    fr.close();
                } catch (IOException e1) {
                    Debug.report(e1);
                }
            }
        }
        else if(f.isDirectory()){
            Debug.debug("\t --> is a directory: "+f.getAbsolutePath());
            File[] files = f.listFiles();
            for (int i = 0; i < files.length; i++) {
                reload(files[i]);
            }
        }
    }
    
    public String getExtension() {
        return extension;
    }
    public void setExtension(String extension) {
        this.extension = extension;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.metadata.ConsultService#addConsultServiceListener(org.cs3.pl.metadata.ConsultServiceListener)
     */
    public void addConsultServiceListener(ConsultServiceListener l) {
        synchronized(listeners){
            if(!listeners.contains(l)){
                listeners.add(l);
            }
        }
        
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.metadata.ConsultService#removeConsultServiceListener(org.cs3.pl.metadata.ConsultServiceListener)
     */
    public void removeConsultServiceListener(ConsultServiceListener l) {
        synchronized(listeners){
            if(listeners.contains(l)){
                listeners.remove(l);
            }
        }
    }
    
    protected void fireConsultDataChanged(ConsultServiceEvent e){
        Vector cloned=null;
        synchronized(listeners){
            cloned=(Vector) listeners.clone();
        }
        for (Iterator it = cloned.iterator(); it.hasNext();) {
            ConsultServiceListener l = (ConsultServiceListener) it.next();
            l.consultDataChanged(e);            
        }
    }
}
