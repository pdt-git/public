package org.cs3.pifcom.codec;

import java.io.ByteArrayOutputStream;
import java.io.DataInput;
import java.io.DataOutputStream;
import java.io.IOException;

import org.cs3.pifcom.PIFComException;

public class Message {

	public final static int OPC_MARK = 0x00;

	public final static int OPC_SKIP = 0x01;

	public final static int OPC_CUT = 0x02;

	public final static int OPC_ABORT = 0x03;

	public final static int OPC_COMPLETE = 0x04;

	public final static int OPC_TIMEOUT = 0x05;

	public final static int OPC_FAIL = 0x06;

	public final static int OPC_BYE = 0x07;

	public final static int OPC_QUERY = 0x08;

	public final static int OPC_BEGIN_NAMES = 0x09;

	public final static int OPC_BEGIN_SOLUTION = 0x0A;

	public final static int OPC_ERROR = 0x0B;

	public final static int OPC_MULTI_COMPLETE = 0x0C;

	public final static int OPC_BINDING = 0x0D;

	public final static int OPC_RESERVED = 0x0E;

	public final static int OPC_PROTOCOL_ERROR = 0x0F;

	public final static int FLC_BODY = 0x08;

	public final static int FLC_RESERVED1 = 0x10;

	public final static int FLC_RESERVED2 = 0x20;

	public final static int FLC_INCOMPLETE = 0x40;

	public final static int FLC_CONTINUATION = 0x80;

	private static final int MASK_OPC = 0x0F;

	private final int opc;

	private final int ticket;

	private final byte[] body;

	public byte[] getBody(){
		return this.body;
	}

	public static Message read(DataInput in) throws IOException {
		int opc = -1;
		boolean incomplete = true;
		int ticket = -1;
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		byte[] buf = null;
		while (incomplete) {
			int type = in.readUnsignedByte();
			opc = type & MASK_OPC;

			boolean continuation = 0 != (type & FLC_CONTINUATION);
			incomplete = 0 != (type & FLC_INCOMPLETE);
			boolean body = 0 != (type & FLC_BODY);
			if (ticket == -1) { // first message fragment
				if (continuation) {
					throw new PIFComException(
							"Message starts with continuation");
				}
				ticket = in.readUnsignedShort();
			} else if (!continuation) {
				throw new PIFComException("Continuation expecetd");
			}
			int bodylen = 0;
			if (body) {
				if (incomplete) {
					bodylen = 0xFFFF;
				} else {
					bodylen = in.readUnsignedShort();
				}
			} else if (incomplete) {
				throw new PIFComException("Message is incomplete AND bodyless");

			}
			if (buf == null) {
				// this is only called more than once if the first fragment
				// has size 0xFFFF, which is maximum.
				// So this buffer will always be big enough.
				buf = new byte[bodylen];
			}
			in.readFully(buf, 0, bodylen);
			out.write(buf, 0, bodylen);
		}
		return new Message(opc, ticket, out.toByteArray());
	}

	public Message(int opc, int ticket, byte[] body) {
		this.opc = opc;
		this.ticket = ticket;
		this.body = body;

	}

	public void write(DataOutputStream out) throws IOException {
		int todo = body.length;
		int done = 0;

		while (todo > 0) {
			int type = opc;
			boolean haveTicket = true;
			boolean haveLength = true;
			int bodyLength = todo;
			if (todo > 0xFFFF) {
				type += FLC_INCOMPLETE;
				haveLength = false;
				bodyLength = 0xFFFF;
			}
			if (0 == (opc & FLC_BODY)) {
				haveLength = false;
			}
			if (done > 0) {
				type += FLC_CONTINUATION;
				haveTicket = false;
			}
			out.writeByte(type);
			if (haveTicket) {
				out.writeShort(ticket);
			}
			if (haveLength) {
				out.writeShort(bodyLength);
			}
			out.write(body, done, bodyLength);
			todo -= bodyLength;
			done += bodyLength;
		}
	}
}
