package org.cs3.pl.common;
import java.io.IOException;
import java.net.ServerSocket;

/**
 * contains static methods that do not quite fit anywhere else :-)=
 */
public class Util {
	public static boolean probePort(int port){
	    try {
            ServerSocket ss = new ServerSocket(port);
            ss.close();
        } catch (IOException e1) {
            return true;
        }        
	    return false;   
    }	
}
