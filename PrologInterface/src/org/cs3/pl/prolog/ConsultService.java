package org.cs3.pl.prolog;

import java.io.PrintStream;

/**
 * provides write access to (virtual) resources which are consulted by the
 * prolog system.
 */
public interface ConsultService {
    
    /**
     * provides a stream to which clauses can be written. The supplied data will
     * be consulted as "virtual" file with the symbolic name specified by
     * setSymbol as soon as the stream is closed or eof is written.
     * 
     * 
     * @return an output stream to which clauses can be written.
     */
    public PrintStream getOutputStream(String symbol);

    /**
     * @return true if and only if data has been consulted using the given
     *               symbolic filename
     */
    public boolean isConsulted(String s);

    /**
     * marks the given symbol as deleted, effectively removing the facts
     * previously bound to it.
     * <p>
     * Note: i do not know of any "clean" way to implement this. The contract
     * for this method is. that it will try to emulate the effect, e.g. by
     * consulting an emty stream and marking the symbol in an apropiate way.
     * <p>
     * If <code>s</code> is not a symbolic filename consulted with <b>this
     * </b> ConsultService, the call has no effect.
     */
    public void unconsult(String s);

    /**
     * get the symbol's time stamp
     * 
     * @param s
     *                    a symbolic file name.
     * @return the number of milliseconds between 00:00:00 GMT, January 1, 1970
     *               and instance the last modification was made to the data consulted as symbolic
     *               file s, or -1 If the information is not available for what reasons ever.
     *               
     *  
     */
    public long getTimeStamp(String s);

    /**
     * @param provider
     */
    public void addConsultServiceListener(ConsultServiceListener provider);
    public void removeConsultServiceListener(ConsultServiceListener provider);
}
