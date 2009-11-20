package org.cs3.pifcom;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pifcom.codec.CTermMessage;
import org.cs3.pifcom.codec.Message;
import org.cs3.pifcom.codec.NameMessage;
import org.cs3.pifcom.codec.UIntMessage;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;

public class PIFComSession implements PrologSession {

	private AbstractPrologInterface pif;

	private PIFComConnection connection;

	private int flags;

	
	public PIFComSession(PIFComConnection connection,
			AbstractPrologInterface pif, int flags) {
		this.connection = connection;
		this.pif = pif;
		this.flags=flags;
	}


	public PrologInterface getPrologInterface() {
		return pif;
	}

	public List queryAll(String query) throws PrologException,
			PrologInterfaceException {
		return queryAll(query,this.flags);
	}

	public List queryAll(String query, int flags) throws PrologException,
			PrologInterfaceException {
		PLUtil.checkFlags(flags);
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed.");
		}
		if (!query.endsWith(".")) {
			query = query + ".";
		}
		Vector<Map> result = new Vector<Map>();
		int ticket = connection.getNewTicket();

		try {

			connection.writeBatchMessage(Message.query(ticket, query));
			connection.flushBatch();
			Message m = join(ticket);

			String[] varNames = null;
			int i = 0; // iterates over variables
			Map<String, Object> solution = null;
			while (true) {
				if (m.getTicket() != ticket) {
					throw new PrologInterfaceException("wrong ticket");
				}
				switch (m.getOpCode()) {
				case Message.OPC_CUT:
				case Message.OPC_FAIL:
					return result;
				case Message.OPC_BEGIN_SOLUTION:
					varNames = new String[((UIntMessage) m).getIntValue()];
					break;
				case Message.OPC_NAME:
					varNames[i++] = ((NameMessage) m).getStringValue();
					break;
				case Message.OPC_BINDING:
					if (solution == null) {
						solution = new HashMap<String, Object>();
						result.add(solution);
					}
					solution.put(varNames[i++],
							PIFComUtils.processBinding((CTermMessage) m,flags));
					break;
				case Message.OPC_EMPTY:
					if (solution == null) {
						solution = new HashMap<String, Object>();
						result.add(solution);
					}
					break;
				case Message.OPC_ERROR:
				case Message.OPC_PROTOCOL_ERROR:
					throw new PrologException(((CTermMessage) m)
							.getStringValue());
				default:
					throw pif.error(new PrologInterfaceException(
							"unexpected Message"));
				}
				if (i == varNames.length) {
					solution = null;
					i = 0;
				}
				m = connection.readMessage();
			}
		} catch (IOException e) {
			throw pif.error(e);

		}
	}
	public Map queryOnce(String query) throws PrologException,
	PrologInterfaceException {
		return queryOnce(query,this.flags);
	}
	
	public Map queryOnce(String query, int flags) throws PrologException,
			PrologInterfaceException {
		PLUtil.checkFlags(flags);
		if (query.endsWith(".")) {
			query = query.substring(0, query.length() - 1);
		}
		query = "once((" + query + ")).";
		List<Map> l = queryAll(query,flags);
		if (l.isEmpty()) {
			return null;
		}
		return l.get(0);
	}

	public void dispose() {
		connection.dispose();
	}

	public boolean isDisposed() {
		return connection == null || connection.isDisposed();
	}

	
	

	

	

	private Message join(int ticket) throws IOException {
		Message m = connection.readMessage();
		while (m.getTicket() != ticket) {
			m = connection.readMessage();
		}
		return m;
	}

	public String getProcessorThreadAlias() throws PrologInterfaceException {
		return connection.getThreadAlias();
	}

}
