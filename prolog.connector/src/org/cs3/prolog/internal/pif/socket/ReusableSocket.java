/* $LICENSE_MSG$(ld) */

/*
 */
package org.cs3.prolog.internal.pif.socket;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;

import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.common.logging.LogBuffer;
import org.cs3.prolog.common.logging.SimpleLogBuffer;

public class ReusableSocket extends Socket implements Reusable {

    private LogBuffer logBuffer = new SimpleLogBuffer();

    public ReusableSocket(String host, int port) throws UnknownHostException,
            IOException {
        super(host, port);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#reuse()
     */
    @Override
	public void reuse() {
        logBuffer.log("socket","reuse");;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#destroy()
     */
    @Override
	public void destroy() {
        logBuffer.log("socket","destroy");
        try {
            close();
        } catch (IOException e) {
	        Debug.report(e);
	        throw new RuntimeException(e.getMessage());
        }
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#recylce()
     */
    @Override
	public void recylce() {
        logBuffer.log("socket","recycle");
    }

    public LogBuffer getLogBuffer() {
        return this.logBuffer;
    }

}

