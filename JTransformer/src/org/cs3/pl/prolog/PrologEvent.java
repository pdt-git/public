package org.cs3.pl.prolog;

import java.util.EventObject;

public class PrologEvent extends EventObject {

    private String data = null;

    private IPrologClient client = null;

    private int stream = -1;

    /**
     * @return new available data, or null if none is available.
     */
    public String getData() {
        return data;
    }

    /**
     * @return Returns the client to which this event does apply, or null, if
     *               the client is unknown.
     */
    public IPrologClient getClient() {
        return client;
    }

    /**
     * @return Returns the stream on which new data is available or
     *               NO_STREAM
     */
    public int getStream() {
        return stream;
    }

    public PrologEvent(Object source, String text, int stream) {
        super(source);
        this.data = text;
        this.stream=stream;
    }
    
    public PrologEvent(IPrologClient client){
        super(client);
        this.client=client;
    }

}