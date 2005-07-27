package org.cs3.pl.prolog;

import java.io.PrintStream;

/**
 * @deprecated ld: i don't like this interface any more. It voices promises we
 *                   can hardly keep (at least for now). We WILL have a meens to
 *                   consult into a stream, but we will NOT have the other
 *                   functionality in suggested by this interface. In particular, the
 *                   record/reload mechanism turned out to be not as performant as
 *                   expected.
 * 
 *  
 */
public interface ConsultService {

    /**
     * provides a stream to which clauses can be written. The supplied data will
     * be consulted as "virtual" file with the symbolic name specified by
     * setSymbol as soon as the stream is closed or eof is written.
     * 
     * @deprecated avoid it if you can. this method will be moved soon.
     * @return an output stream to which clauses can be written.
     */
    public PrintStream getOutputStream(String symbol);

    /**
     * @deprecated cwe simply do not know!
     * @return true if and only if data has been consulted using the given
     *            symbolic filename
     */
    //public boolean isConsulted(String s);

    /**
     * @deprecated cannot give an adequat implementation. marks the given symbol
     *                   as deleted, effectively removing the facts previously bound
     *                   to it.
     *                   <p>
     *                   Note: i do not know of any "clean" way to implement this. The
     *                   contract for this method is. that it will try to emulate the
     *                   effect, e.g. by consulting an emty stream and marking the
     *                   symbol in an apropiate way.
     *                   <p>
     *                   If <code>s</code> is not a symbolic filename consulted with
     *                   <b>this </b> ConsultService, the call has no effect.
     */
    //public void unconsult(String s);

    /**
     * @deprecated cannot give an adequat implementation. get the symbol's time
     *                   stamp
     * 
     * @param s
     *                a symbolic file name.
     * @return the number of milliseconds between 00:00:00 GMT, January 1, 1970
     *            and instance the last modification was made to the data consulted
     *            as symbolic file s, or -1 If the information is not available for
     *            what reasons ever.
     * 
     *  
     */
    //public long getRecordTimeStamp(String s);

    /**
     * @param provider
     * @deprecated use
     *                   IPrologInterface.addPrologInterfaceListener(IPrologInterface.SUBJECT_CONSULT, _)
     */
    public void addConsultServiceListener(ConsultServiceListener provider);

    /**
     * @param provider
     * @deprecated use
     *                   IPrologInterface.addPrologInterfaceListener(IPrologInterface.SUBJECT_CONSULT, _)
     */
    public void removeConsultServiceListener(ConsultServiceListener provider);

    /**
     * reload recorded data. If the ConsultService supports recording of
     * consulted streams, it will try to reload any existing records. Otherwise
     * (or if no records are available) this method does nothing.
     * 
     * @throws IOException
     */
    // public void reload() throws IOException;
    /**
     * clear recorded data. if the ConsultService supports recording of
     * consulted streams, this will clear all existing records it currently
     * keeps.
     */
    //public void clearRecords();

    /**
     * @return true if and only if this consult service supports recording and
     *            recording is currently enabled.
     */
    //public boolean isRecording();

    /**
     * if this consult service supports recording, this method can be used to
     * switch it on or off.
     */
    //public void setRecording(boolean val);

    /**
     * 
     * @return true if the consult service appends consulted data to a already
     *            exisiting record.
     */
   // public boolean isAppendingRecords();

    /**
     * if this consult service supports appending to existing records, this
     * method can switch it on and off.
     */
    //public void setAppendingRecords(boolean val);
}
