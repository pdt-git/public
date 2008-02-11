package org.cs3.pifcom.codec;

public class UIntMessage extends Message {

	private int value;

	public UIntMessage(int opc, int ticket, byte[] body) {
		super(opc, ticket, body);
		value += body[0] << 24;
		value += body[1] << 16;
		value += body[2] << 8;
		value += body[3];
	}
	
	public int getIntValue(){
		return value;
	}
}
