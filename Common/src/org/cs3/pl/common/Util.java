package org.cs3.pl.common;
import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;

/**
 * contains static methods that do not quite fit anywhere else :-)=
 */
public class Util {
	public static boolean probePort(int port,String eof){
    Socket s=null;
		try {
            s= new Socket("localhost",port);
            s.shutdownInput();
        }
        catch (UnknownHostException e) {
            Debug.report(e);
            return false;
        } catch (IOException e) {
        
            return false;
        }
        try{
            Thread.sleep(100);
            if(s.isConnected()&& ! s.isOutputShutdown()){
            	s.getOutputStream().write(eof.getBytes());
            }
            if(!s.isClosed()){
                s.shutdownOutput();
                s.close();
            }
        } catch (UnknownHostException e) {
            Debug.report(e);            
        } catch (IOException e) {
        	Debug.warning("The following exception is propably harmless, still we should find out, how to avoid it.");
        	Debug.report(e);
        } catch (InterruptedException e) {
            Debug.report(e);
        }
        return true;        
    }
}
