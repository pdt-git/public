package org.cs3.pl.common;
import java.io.IOException;
import java.net.ServerSocket;
import java.util.Iterator;
import java.util.Map;

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
	public static String prettyPrint(Map r) {
        if (r != null) {
            StringBuffer sb = new StringBuffer();
            for (Iterator it = r.keySet().iterator(); it.hasNext();) {
                String key = (String) it.next();
                String val = (String) r.get(key);
                sb.append(key + "-->" + val+"\n");
                
            }
            return sb.toString();
        }
        return "";
    }
	
	public static String prettyPrint(Object[] a) {
	    if(a==null){
	        return "";
	    }
	    StringBuffer sb=new StringBuffer();
	    sb.append("{");
	    for (int i = 0; i < a.length; i++) {
	        if(i>0){
	            sb.append(", ");
	        }
            sb.append(a[i].toString());
        }
	    sb.append("}");
	    return sb.toString();
	}
}
