package org.cs3.pifcom;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pifcom.codec.CTermMessage;
import org.cs3.pifcom.codec.Message;
import org.cs3.pifcom.codec.NameMessage;
import org.cs3.pifcom.codec.UIntMessage;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession2;

public class PIFComSession implements PrologSession2 {

	private PrologInterface pif;
	
	private Option[] options;
	private boolean canonical;
	private boolean interpreteLists;
	private int nextTicket=0;

	private PIFComConnection connection;

	private static final String OPT_CANONICAL = "socketsession.canonical";

	private static final String OPT_INTERPRETE_LISTS = "socketsession.interprete_lists";

	
	public void endQuery() throws PrologException, PrologInterfaceException {
		throw new PrologInterfaceException("not implemented");

	}

	public PrologInterface getPrologInterface() {
		return pif;
	}

	public Map next() throws PrologException, PrologInterfaceException {
		throw new PrologInterfaceException("not implemented");
	}

	public Map query(String query) throws PrologException,
			PrologInterfaceException {
		throw new PrologInterfaceException("not implemented");
	}

	public List queryAll(String query) throws PrologException,
			PrologInterfaceException {
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
							processBinding((CTermMessage) m));
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
					throw new PrologInterfaceException("unexpected Message");
				}
				if(i==varNames.length){
					solution=null;
					i=0;
				}
				m=connection.readMessage();
			}
		} catch (IOException e) {
			throw new PrologInterfaceException(e);
		}		
	}

	
	

	public Map queryOnce(String query) throws PrologException,
			PrologInterfaceException {
		if(query.endsWith(".")){
			query=query.substring(0,query.length()-1);
		}
		query="once("+query+").";
		List<Map> l = queryAll(query);
		if(l.isEmpty()){
			return null;
		}
		return l.get(0);
	}

	public void dispose() {
		connection.dispose();
	}

	public boolean isDisposed() {
		return connection.isDisposed();
	}

	public Option[] getOptions() {
		if (options == null) {
			options = new Option[] {
					new SimpleOption(OPT_CANONICAL, "canonical values",
							"if set, the session will answer canonical terms",
							Option.FLAG, "false"),
					new SimpleOption(OPT_INTERPRETE_LISTS, "interprete lists",
							"if set, the session will use (nested) java.util.List instances to represent"
									+ " prolog list terms.", Option.FLAG,
							"true") };
		}
		return options;
	}

	/**
	 * this implementation does nothing.
	 */
	public void reconfigure() {
		;

	}

	public String getPreferenceValue(String id, String string) {
		if (OPT_CANONICAL.equals(id)) {
			return canonical ? "true" : "false";
		}else if(OPT_INTERPRETE_LISTS.equals(id)){
			return interpreteLists ? "true" :"false";
		}
		throw new IllegalArgumentException("unkown option id: " + id);
	}

	public void setPreferenceValue(String id, String value) {

		if (OPT_CANONICAL.equals(id)) {
			canonical = Boolean.valueOf(value).booleanValue();			
		} else if (OPT_INTERPRETE_LISTS.equals(id)) {
			interpreteLists = Boolean.valueOf(value).booleanValue();			
		} else {
			throw new IllegalArgumentException("unkown option id: " + id);
		}
	}
	private Object processBinding(CTermMessage m) {
		if(canonical){
			return m.getCTermValue();
		}
		return m.getStringValue();
		
		//FIXME: handle lists
	}

	private Message join(int ticket) throws IOException {
		Message m = connection.readMessage();
		while (m.getTicket() != ticket) {
			m = connection.readMessage();
		}
		return m;
	}

	

	

}
