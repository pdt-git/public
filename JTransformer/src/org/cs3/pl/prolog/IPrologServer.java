package org.cs3.pl.prolog;

import java.util.Hashtable;
/**
 * contains all methods of the server that are called by the client via rpc.
 */
public interface IPrologServer {
    public Boolean consultLibrary(String module) ;

    public Hashtable next() ;

    public Hashtable query(String text) ;
}
