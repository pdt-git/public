/*
 */
package org.cs3.pl.prolog.internal.xml;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.Socket;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.InputStreamSpy;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.internal.Reusable;

/**
 */
public class XMLClient implements Reusable {

    public static final String BINDING = "binding";

    public static final String COMMAND = "command";

    public static final String EVENT = "event";

    public static final String EXCEPTION = "exception";

    public static final String FAILED = "failed";

    public static final String FILE = "file";

    public static final String NAME = "name";

    public static final String REPLY = "reply";

    public static final String REQUEST = "request";

    public static final String SESSION = "session";

    public static final String SUBJECT = "subject";

    public static final String TUPLE = "tuple";

    public static final String TYPE = "type";
    
    public static final String OK= "ok";

    private boolean destroyed;

    private Vector exceptions = new Vector();

    private Vector failed = new Vector();

    private HashMap fileProviders = new HashMap();

    private InputStreamSpy in;

    private HashMap listenerLists;

    private PrintStream out;

    private int port;

    private XMLStreamReader reader;

    private Socket socket;

    private HashMap solution = new HashMap();

    private Vector solutions = new Vector();

    public XMLClient(int port) {
        this.port = port;
        
    }

    private boolean available() throws IOException, XMLStreamException {
        return in.available() > 0
                || reader.getLocation().getCharacterOffset() < in.getMypos();
    }

    protected void connect() throws XMLStreamException, IOException {
        this.socket = new Socket("localhost", port);
        out = new PrintStream(new BufferedOutputStream(socket.getOutputStream()));
        XMLInputFactory factory = XMLInputFactory.newInstance();
        in = new InputStreamSpy(socket.getInputStream(), System.out);

        InputStreamReader isr = new InputStreamReader(in);
        reader = factory.createXMLStreamReader(isr);
        skip_until(SESSION);
        dispatch_until(OK);
    }

    public boolean ok(){
        return OK.equals(reader.getLocalName());
    }
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.internal.Reusable#destroy()
     */
    public void destroy() {
        if (!isDestroyed()) {
            try {
                writeln("bye");
                dispatch_all();
                if (!isDestroyed()) {
                    throw new IllegalStateException("could not kill client");
                }
            } catch (XMLStreamException e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * dispatch elements until the end of session, then close the client.
     * 
     * @throws XMLStreamException
     */
    private void dispatch_all() throws XMLStreamException {
        dispatch_until(null);
    }

    
    public void dispatch_until_next(String target) throws XMLStreamException{
        reader.nextTag();
        dispatch_until(target);
    }
    
    /**
     * keep dispatching elements (including the current) until the cursor points
     * on a start tag with the local name <code>target</code>. the target
     * element itself will not be dispatched. If the
     * <code>&lt;/session&gt;</code> end tag is encountered, the client is
     * closed.
     * 
     * <p>
     * precondition: cursor points on a element start or end tag.
     * <p>
     * postcondition: either the client is closed or the cursor points on the
     * target element begin tag.
     * 
     * @param target
     * @throws XMLStreamException
     */
    public void dispatch_until(String target) throws XMLStreamException {
        while (target == null 
                ||! target.equals(reader.getLocalName())
                ||  XMLStreamConstants.START_ELEMENT!=reader.getEventType()) {
            
            Debug.debug("during dispatch_until: type="+reader.getEventType()+", lname="+reader.getLocalName());
            if (reader.getEventType() == XMLStreamConstants.START_ELEMENT
                    || reader.getLocalName().equals(SESSION)) {
                dispatch_current();
                if (isDestroyed()) {
                    return;
                }
            }
            reader.nextTag();
        }

    }

    public void skip_until(String target) throws XMLStreamException {
        while (XMLStreamConstants.START_ELEMENT != reader.getEventType()
                || ! reader.getLocalName().equals(target)) {
            Debug.debug("skipping: type="+reader.getEventType()+", lname="+reader.getLocalName());
            reader.next();
        }
        Debug.debug("after skip: type="+reader.getEventType()+", lname="+reader.getLocalName());
    }

    /**
     * read and handle the current element. expects the cursor to point on a
     * start element tag or the session end tag <code>&lt;/session&gt;</code>
     * After the call, the cursor will point to the respective end tag, or, if
     * the session end tag was read, the client will be closed.
     * 
     * @return
     * @throws IOException
     * @throws XMLStreamException
     */
    public void dispatch_current() throws XMLStreamException {
        
        String lname = reader.getLocalName();
        int eventType = reader.getEventType();
        Debug.debug("dispatch: lname=" + lname + " type=" + eventType);
        if (eventType == XMLStreamReader.END_ELEMENT) {
            if (SESSION.equals(lname)) {
                finish();
            } else {
                throw new XMLStreamException("dispatch did not expect  tag </ "
                        + lname + ">", reader.getLocation());
            }
        } else if (eventType == XMLStreamConstants.START_ELEMENT) {

            if (EXCEPTION.equals(lname)) {
                handleException();
            } else if (FAILED.equals(lname)) {
                handleFailed();
            } else if (TUPLE.equals(lname)) {
                handleTuple();
            } else if (EVENT.equals(lname)) {
                handleEvent();
            } else if (REQUEST.equals(lname)) {
                ;
            } else if (REPLY.equals(lname)) {
                ;
            } else if (SESSION.equals(lname)) {
                ;
            }else if (OK.equals(lname)) {
                ;
            } else {
                throw new XMLStreamException("dispatch did not expect tag: <"
                        + lname + ">", reader.getLocation());
            }
        } else {
            Debug.warning("dispatchOne got stuck in the middle of nowhere. ");
        }
        ;
    }

    public void clear(){
        solution.clear();
        solutions.clear();
        failed.clear();
        exceptions.clear();
    }
    

    public void writeln(String command) throws XMLStreamException {
        out.println(command);
    }

    public void write(String command) throws XMLStreamException {
        out.print(command);
    }

    /**
     * @throws XMLStreamException
     * @throws IOException
     *  
     */
    private void finish() throws XMLStreamException {
        if(isDestroyed()){
            Debug.warning("redundant call to finish,");
            return;
        }
        while (reader.getEventType() != XMLStreamConstants.END_DOCUMENT) {
            reader.next();
            Debug.debug("finish: lname=" + reader.getLocalName() + " type=" + reader.getEventType());
        }
        try {
            reader.close();
            in.close();
            out.close();
            socket.close();
            Debug
                    .info("Good night sweet prince. May flights of angles sing thee to thy rest.");
        } catch (IOException e) {
            throw new XMLStreamException("IO problems", e);
        }
    }

    /**
     * @param subject2
     * @param string
     */
    private void fireUpdate(String subject, String event) {
        Vector listeners = (Vector) listenerLists.get(subject);
        if (listeners == null) {
            return;
        }
        XMLClientEvent e = new XMLClientEvent(this, subject, event);

        Vector cloned = null;
        synchronized (listeners) {
            cloned = (Vector) listeners.clone();
        }
        for (Iterator it = cloned.iterator(); it.hasNext();) {
            XMLClientListener l = (XMLClientListener) it.next();
            l.update(e);
        }
    }

    /**
     * @throws XMLStreamException
     *  
     */
    private void handleBinding() throws XMLStreamException {
        String name = reader.getAttributeValue(null, NAME);
        String value = reader.getElementText();
        solution.put(name, value);
    }

    private void handleEvent() throws XMLStreamException {
        String subject = reader.getAttributeValue(null, SUBJECT);
        String event = reader.getElementText();
        fireUpdate(subject, event);
    }

    private void handleException() throws XMLStreamException {
        String exception = reader.getElementText();
        exceptions.add(exception);
    }

    /**
     * @throws XMLStreamException
     *  
     */
    private void handleFailed() throws XMLStreamException {
        String goal = reader.getElementText();
        failed.add(goal);
    }

    /**
     * @throws XMLStreamException
     *  
     */
    private void handleTuple() throws XMLStreamException {
        solution = new HashMap();
        int type;
        while ((type = reader.nextTag()) == XMLStreamConstants.START_ELEMENT) {
            String lname = reader.getLocalName();
            if (FAILED.equals(lname)) {
                handleFailed();
            } else if (EXCEPTION.equals(lname)) {
                handleException();
            } else if (BINDING.equals(lname)) {
                handleBinding();
            } else {
                throw new XMLStreamException(
                        "handleTuple did not expect start tag: <" + lname + ">",
                        reader.getLocation());
            }
        }
        if (type == XMLStreamConstants.END_ELEMENT) {
            String lname = reader.getLocalName();
            if (!TUPLE.equals(lname)) {
                throw new XMLStreamException(
                        "tuple must be terminated  </tuple> tag " + "not </ "
                                + lname + ">", reader.getLocation());
            }
            solutions.add(solution);
        } else {
            throw new XMLStreamException(
                    "handleTuples did not expect this event type here:" + type,
                    reader.getLocation());
        }
    }

    /**
     * @return Returns the destroyed.
     */
    private boolean isDestroyed() {
        return socket.isClosed();
    }

    /**
     * @param type2
     * @param subject2
     * @param string
     */
    private void provideContent(String type, String subject, String request) {
        if (type == FILE) {
            IFileProvider provider = (IFileProvider) fileProviders.get(subject);
            if (provider == null) {
                out.println("no");
                return;
            }
            InputStream in = provider.getContent(subject, request);
            if (in == null) {
                out.println("no");
                return;
            }
            out.println("yes");
            try {
                Util.copy(in, out);
                in.close();
            } catch (IOException e) {
                Debug.report(e);
                throw new RuntimeException(e);
            }
        }

    }

    private void beginReply() {
        solutions.clear();
    }

    private void beginRequest() {
        String type = reader.getAttributeValue(null, TYPE);
        String name = reader.getAttributeValue(null, NAME);
        String subject = reader.getAttributeValue(null, SUBJECT);
        provideContent(type, subject, name);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.internal.Reusable#recylce()
     */
    public void recylce() {
        if (isDestroyed()) {
            throw new IllegalStateException("cannot reuse a destroyed client");
        }
        try {
            exceptions.clear();
            failed.clear();
            if (fileProviders.isEmpty() && listenerLists.isEmpty()) {
                return;
            } else {
                fileProviders.clear();
                listenerLists.clear();
                writeln("reset");
                dispatch_until(OK);
                
            }
        } catch (XMLStreamException e) {
            throw new RuntimeException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.internal.Reusable#reuse()
     */
    public void reuse() {
        if (isDestroyed()) {
            throw new IllegalStateException("cannot reuse a destroyed client");
        }
    }

    /**
     * @return Returns the exceptions.
     */
    public Vector getExceptions() {
        return exceptions;
    }

    /**
     * @return Returns the failed.
     */
    public Vector getFailed() {
        return failed;
    }

    /**
     * @return Returns the port.
     */
    public int getPort() {
        return port;
    }

    /**
     * @return Returns the solution.
     */
    public HashMap getSolution() {
        return solution;
    }

    /**
     * @return Returns the solutions.
     */
    public Vector getSolutions() {
        return solutions;
    }

    public void printState(PrintStream out) {
        out.println("current tuple:");
        out.println(Util.prettyPrint(solution));
        out.println("all tuples:");
        for (Iterator it = solutions.iterator(); it.hasNext();) {
            Map t = (Map) it.next();
            out.println(Util.prettyPrint(t));
        }

        out.println("all exceptions:");
        for (Iterator it = exceptions.iterator(); it.hasNext();) {
            String t = (String) it.next();
            out.println(t);
        }

        out.println("all failed goals:");
        for (Iterator it = failed.iterator(); it.hasNext();) {
            String t = (String) it.next();
            out.println(t);
        }
    }

    /**
     * for testing purposes
     * 
     * @param args
     * @throws IOException
     * @throws XMLStreamException
     */
    public static void main(String[] args) throws XMLStreamException,
            IOException {
        Debug.setDebugLevel(Debug.LEVEL_DEBUG);
        final XMLClient client = new XMLClient(9944);
        client.connect();
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        while (!client.isDestroyed()) {
            try {
                String line = in.readLine();
                client.writeln(line);
                client.dispatch_until_next(OK);           
                client.printState(System.out);
            } catch (XMLStreamException e) {
                Debug.report(e);
            }
        }
    }

    /**
     * @return Returns the out.
     */
    public PrintStream getOut() {
        return out;
    }
}
