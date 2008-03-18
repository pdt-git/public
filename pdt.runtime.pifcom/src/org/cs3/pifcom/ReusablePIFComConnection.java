/**
 * 
 */
package org.cs3.pifcom;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;

import org.cs3.pifcom.codec.Message;
import org.cs3.pifcom.codec.NameMessage;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.internal.Reusable;
import org.cs3.pl.prolog.internal.ReusablePool;

abstract class ReusablePIFComConnection implements PIFComConnection, Reusable {
	/**
	 * 
	 */

	private int nextTicket = 0;
	private final DataInputStream in;
	private final DataOutputStream out;
	private final int udpSlavePort;
	private final Socket clientSocket;
	private final InetAddress host;
	private boolean disposed;
	private final String processorThreadAlias;
	private final ReusablePool pool;
	private final DatagramSocket udpSocket;

	public ReusablePIFComConnection(DatagramSocket udpSocket, InetAddress host,
			int port, ReusablePool pool) throws UnknownHostException,
			IOException {

		this.udpSocket = udpSocket;
		this.pool = pool;
		// connect
		clientSocket = new Socket(host, port);
		in = new DataInputStream(new BufferedInputStream(clientSocket
				.getInputStream()));

		out = new DataOutputStream(new BufferedOutputStream(clientSocket
				.getOutputStream()));
		this.host = host;
		clientSocket.setSoTimeout(2000);
		NameMessage m;
		try {
			udpSlavePort = in.readUnsignedShort();
			m = (NameMessage) readMessage();
		} finally {
			clientSocket.setSoTimeout(0);
		}
		processorThreadAlias = m.getStringValue();
	}

	@Override
	public void dispose() {
		disposed = true;
		if (pool != null) {
			pool.recycle(this);
		} else {
			destroy();
		}
	}

	@Override
	public void flushBatch() throws IOException {
		out.flush();

	}

	@Override
	public int getNewTicket() {
		if (nextTicket == 0xFFFF) {
			nextTicket = 0;
			return 0xFFFF;
		}
		return nextTicket++;
	}

	@Override
	public String getThreadAlias() {
		return processorThreadAlias;
	}

	@Override
	public boolean isDisposed() {

		return disposed;
	}

	@Override
	public Message readMessage() throws IOException {

		return Message.read(in);
	}

	@Override
	public void sendControlMessage(Message m) throws IOException {
		m.send(udpSocket, host, udpSlavePort);
	}

	@Override
	public void writeBatchMessage(Message m) throws IOException {
		m.write(out);
	}

	@Override
	public void destroy() {
		try {
			writeBatchMessage(Message.bye(getNewTicket()));
			flushBatch();
			while (readMessage().getOpCode() != Message.OPC_BYE)
				;
			in.close();
			out.close();
			clientSocket.close();
		} catch (IOException e) {
			;// not much I can do.
		}

	}

	@Override
	public void recylce() {
	}

	@Override
	public void reuse() {
		disposed = false;
		try {
			int ticket = getNewTicket();
			writeBatchMessage(Message.mark(ticket));
			flushBatch();
			while (readMessage().getTicket() != ticket)
				;
		} catch (IOException e) {
			try {
				error(e);
			} catch (PrologInterfaceException e1) {
				Debug.rethrow(e1);
			}
		}

	}

	protected abstract void error(Throwable e) throws PrologInterfaceException;

}