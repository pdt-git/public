package org.cs3.pl.prolog.internal.socket;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.ConsultServiceEvent;
import org.cs3.pl.prolog.ConsultServiceListener;
import org.cs3.pl.prolog.PrologException;

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

    private SocketClient consultClient;

    private String extension = ".pl";

    private Vector listeners = new Vector();

    private int port = 5624;

    private File prefix = null;
    
    private boolean keepRecords=true;

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.metadata.ConsultService#addConsultServiceListener(org.cs3.pl.metadata.ConsultServiceListener)
     */
    public void addConsultServiceListener(ConsultServiceListener l) {
        synchronized (listeners) {
            if (!listeners.contains(l)) {
                listeners.add(l);
            }
        }

    }

   

    public void connect() throws IOException {        
        consultClient=new SocketClient("localhost",port);      
    }

    public void disconnect() {
        try {
            consultClient.close();
        } catch (IOException e) {
            Debug.report(e);
        }
    }

    protected void fireConsultDataChanged(ConsultServiceEvent e) {
        Vector cloned = null;
        synchronized (listeners) {
            cloned = (Vector) listeners.clone();
        }
        for (Iterator it = cloned.iterator(); it.hasNext();) {
            ConsultServiceListener l = (ConsultServiceListener) it.next();
            l.consultDataChanged(e);
        }
    }

    public String getExtension() {
        return extension;
    }

    /**
     * creates an absolute os filesystem path from an unprefixed symbol
     * 
     * @param s
     * @return
     */
    //	private String getFileName(String s) {
    //		String symbol = getPrefixedSymbol(s);
    //		URI uri = null;
    //		try {
    //			uri = new URI(symbol);
    //
    //		} catch (URISyntaxException e) {
    //			throw new IllegalArgumentException(
    //					"prefixed symbol does not form a valid uri string: "+symbol);
    //		}
    //		String filename = symbol.replaceAll("/", File.separator);
    //		return filename;
    //	}
    private File getFile(String unprefixedSymbol) {
        if (unprefixedSymbol.startsWith("/")) {
            unprefixedSymbol = unprefixedSymbol.substring(1);
        }
        URI prefixURI = prefix.toURI();
        prefixURI.relativize(prefixURI);
        URI uri = prefixURI.resolve(unprefixedSymbol);
        return new File(uri);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.metadata.ConsultService#getOutputStream()
     */
    public PrintStream getOutputStream(String s) {
        return getOutputStream_impl(s, keepRecords);
    }

    protected PrintStream getOutputStream_impl(String s, boolean keepRecord) {
        String symbol = getPrefixedSymbol(s);

        Debug.debug("prefix: " + prefix + ", s: " + s);
        String string = "";
        try {            
            ConsultOutputStream stream = new ConsultOutputStream(consultClient,
                    getPrefixedSymbol(s));
            if (keepRecord) {
                File file = getFile(s);
                String filename = file.getAbsolutePath();
                Debug.debug("record filename:" + filename);
                if (!file.exists()) {
                    file.getParentFile().mkdirs();
                    file.createNewFile();
                }
                stream.setRecordStream(new BufferedOutputStream(
                        new FileOutputStream(file)));
            }

            stream.addConsultServiceListener(new ConsultServiceListener() {
                 public void consultDataChanged(ConsultServiceEvent e) {
                    fireConsultDataChanged(new ConsultServiceEvent(
                            RecordingConsultService.this, getUnPrefixedSymbol(e
                                    .getSymbol())));
                }
            });
            return new PrintStream(stream);
        } catch (IOException e) {
            Debug.report(e);
            return null;
        }
    }

  

    public int getPort() {
        return port;
    }

    public File getPrefix() {
        return prefix;
    }

    /**
     * creates a prefixed symbol from a file.
     * 
     * @param
     * @return
     */
    private String getPrefixedSymbol(File f) {
        try {
            String symbol = f.toURI().toURL().getFile();

            if (!symbol.startsWith(getPrefixString())) {
                throw new IllegalArgumentException(
                        "the given file is not within my domain, sorry.");
            }
            return symbol;
        } catch (IOException e) {
            Debug.report(e);
            return null;
        }

    }

    /**
     * creates a prefixed symbol from an unprefixed symbol
     * 
     * @param s
     * @return
     */
    private String getPrefixedSymbol(String s) {
        String prefixString = getPrefixString();
        if (s.startsWith("/")) {
            s = s.substring(1);
        }
        String symbol = prefixString.endsWith("/") ? prefixString + s
                : prefixString + "/" + s;
        return symbol;
    }

    private String getPrefixString() {
        if (prefix == null) {
            return "";
        }
        try {
            return prefix.toURI().toURL().getFile();
        } catch (MalformedURLException e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.metadata.ConsultService#getTimeStamp(java.lang.String)
     */
    public long getTimeStamp(String s) {
        if (!isConsulted(s) || ! keepRecords) {
            return -1;
        }

        File file = getFile(s);
        String filename = file.getAbsolutePath();
        if (file.exists()) {
            long t = file.lastModified();
            return t == 0 ? -1 : t;
        }
        return -1;
    }

    private String getUnPrefixedSymbol(File f) {
        return getUnPrefixedSymbol(getPrefixedSymbol(f));
    }

    private String getUnPrefixedSymbol(String prefixedSymbol) {
        return prefixedSymbol.substring(getPrefixString().length());
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.metadata.ConsultService#isConsulted(java.lang.String)
     */
    public boolean isConsulted(String s) {
        try {
            consultClient.lock();
            consultClient.readUntil(SocketClient.GIVE_COMMAND);
            consultClient.writeln(SocketClient.IS_CONSULTED);
            consultClient.readUntil(SocketClient.GIVE_SYMBOL);
            consultClient.writeln(getPrefixedSymbol(s));
            StringBuffer sb = new StringBuffer();
            consultClient.readUntil(SocketClient.OK,sb );            
            return sb.toString().trim().equals(SocketClient.YES);
        } catch (IOException e) {
            Debug.report(e);
            throw new PrologException(e);
        } finally{
            consultClient.unlock();
        }
    }

    public void reload() {
        reload(prefix);
        fireConsultDataChanged(new ConsultServiceEvent(this));
    }

    private void reload(File f) {
        Debug.debug("reload(File) visiting: " + f.getAbsolutePath());
        if (!f.exists()) {
            Debug.debug("\t --> dos not exist: " + f.getAbsolutePath());
            return;
        }

        if (f.isFile()) {
            Debug.debug("\t --> is a file: " + f.getAbsolutePath());
            if (!f.getName().endsWith(getExtension())) {
                Debug.debug("\t --> wrong extension: " + f.getAbsolutePath());
                return;
            }

            try {
                Debug.debug("\t --> loading: " + f.getAbsolutePath());
                FileInputStream in = new FileInputStream(f);
                PrintStream out = getOutputStream_impl(getUnPrefixedSymbol(f),
                        false);
                Util.copy(in, out);
            } catch (Throwable e) {
                Debug.report(e);
            }
        } else if (f.isDirectory()) {
            Debug.debug("\t --> is a directory: " + f.getAbsolutePath());
            File[] files = f.listFiles();
            for (int i = 0; i < files.length; i++) {
                reload(files[i]);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.metadata.ConsultService#removeConsultServiceListener(org.cs3.pl.metadata.ConsultServiceListener)
     */
    public void removeConsultServiceListener(ConsultServiceListener l) {
        synchronized (listeners) {
            if (listeners.contains(l)) {
                listeners.remove(l);
            }
        }
    }

    public void setExtension(String extension) {
        this.extension = extension;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public void setPrefix(File prefix) {
        this.prefix = prefix;
        if (!prefix.exists()) {
            prefix.mkdirs();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.metadata.ConsultService#unconssult(java.lang.String)
     */
    public void unconsult(String s) {
        try {
            consultClient.lock();
            consultClient.readUntil(SocketClient.GIVE_COMMAND);
            consultClient.writeln(SocketClient.UNCONSULT);
            consultClient.readUntil(SocketClient.GIVE_SYMBOL);
            consultClient.writeln(getPrefixedSymbol(s));            
            consultClient.readUntil(SocketClient.OK);                        
        } catch (IOException e) {
            Debug.report(e);
            throw new PrologException(e);
        } finally{
            consultClient.unlock();
        }

        File file = getFile(s);
        String filename = file.getAbsolutePath();
        if (file.exists()&&keepRecords) {
            file.delete();
        }
        fireConsultDataChanged(new ConsultServiceEvent(this, s));
    }
    public boolean isKeepRecords() {
        return keepRecords;
    }
    public void setKeepRecords(boolean keepRecords) {
        this.keepRecords = keepRecords;
    }
}
