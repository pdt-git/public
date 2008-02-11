package org.cs3.pifcom.codec;

import java.nio.charset.Charset;

public class NameMessage extends Message {
	private String stringValue;

	public NameMessage(int opc, int ticket, byte[] body) {
		super(opc, ticket, body);
		this.stringValue=new String(body,Charset.forName("UTF-8"));
	}
	
	public NameMessage(String name,int ticket){
		super(OPC_NAME, ticket, name.getBytes(Charset.forName("UTF-8")));
		this.stringValue=name;
	}

	public String getStringValue() {
		return stringValue;
	}
	
}
