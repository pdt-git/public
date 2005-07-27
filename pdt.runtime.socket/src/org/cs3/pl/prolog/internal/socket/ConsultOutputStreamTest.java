/*
 */
package org.cs3.pl.prolog.internal.socket;

import java.io.PrintStream;

import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologException;

/**
 * Assumes a running consult_server on port 5624
 */
public class ConsultOutputStreamTest extends TestCase {
    public final static int PORT = 5624;
    static final String[] pre={"nicest","insect","scient","incest"};
    private SocketClient socket;
    /*
     * @see TestCase#setUp()
     */
    protected void setUp() throws Exception {
        socket = new SocketClient((String)null,PORT);
    }

    /* (non-Javadoc)
     * @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        socket.close();
    }
    
    public void _testMultipleSymbols() throws Throwable {        
        for (int i=0;i<pre.length;i++){
            String content = getContent(i,pre[i]);
            ConsultOutputStream stream = new ConsultOutputStream(socket,getSymbol(i,pre[i]) );
            PrintStream s = new PrintStream(stream);
            s.print(content);
            try{
            s.close();
            }catch(PrologException e){
                Debug.report(e);
                fail();
            }
        }
    }
    
    
    public void testSyntaxErrorMultipleSymbols() throws Throwable {        
        for (int i=0;i<pre.length;i++){
            String content = getContent(i,pre[i]);
            ConsultOutputStream stream = new ConsultOutputStream(socket,getSymbol(i,pre[i]) );
            PrintStream s = new PrintStream(stream);
            s.print(content);
            s.println(":-stu ss(2001+"+i+").");
            try{
            s.close();
            fail();
            }catch(PrologException e){
                Debug.report(e);                
            }
        }
        Debug.debug("fnum");
    }
    
    public void _testSingleSymbol() throws Throwable {        
        for (int i=0;i<pre.length;i++){
            String content = getContent(i,pre[i]);
            ConsultOutputStream stream = new ConsultOutputStream(socket,getSymbol(0,pre[0]) );
            PrintStream s = new PrintStream(stream);
            s.print(content);
            try{
            s.close();
            }catch(PrologException e){
                Debug.report(e);
                fail();
            }
        }
       
    }
    public void _testEmptyMultipleSymbols() throws Throwable {        
        for (int i=0;i<pre.length;i++){
            String content = getContent(i,pre[i]);
            ConsultOutputStream stream = new ConsultOutputStream(socket,getSymbol(i,pre[i]) );
            PrintStream s = new PrintStream(stream);
            //s.print(content);
            try{
            s.close();
            }catch(PrologException e){
                Debug.report(e);
                fail();
            }
        }
    }
    public void _testEmptySingleSymbol() throws Throwable {        
        for (int i=0;i<pre.length;i++){
            String content = getContent(i,pre[i]);
            ConsultOutputStream stream = new ConsultOutputStream(socket,getSymbol(0,pre[0]) );
            PrintStream s = new PrintStream(stream);
            //s.print(content);
            try{
            s.close();
            }catch(PrologException e){
                Debug.report(e);
                fail();
            }
        }
    }

    
    /**
     * @param i
     * @param string
     * @return
     */
    private String getContent(int i, String string) {
        
        return ":-dynamic wahr/1.\n" + ":-assert(wahr("+i+")).\n"
        + string+"(A) :-\n" + "\twahr(A).";
    }
    
    private String getSymbol(int i, String string){
        return "/fno/fnum/"+string;
    }
}
