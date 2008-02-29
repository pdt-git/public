package org.cs3.pifcom;

import java.io.IOException;

import org.cs3.pifcom.codec.Message;

public interface PIFComConnection {
	public Message readMessage() throws IOException;

	public void writeBatchMessage(Message m) throws IOException;

	public void flushBatch() throws IOException;

	public void sendControlMessage(Message m);

	public boolean isDisposed();

	public void dispose();
	
	public int getNewTicket();

	public String getThreadAlias();
}
