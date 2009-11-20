package org.cs3.pifcom.codec;

import java.io.UnsupportedEncodingException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;

public class NameMessage extends Message {
	private String stringValue;

	public NameMessage(int opc, int ticket, byte[] body) {
		super(opc, ticket, body);
		try {
			// This is only necessary to support Java 5, better new String(body,Charset.forName("UTF-8"))
			this.stringValue=new String(body,"UTF-8");
		} catch (UnsupportedEncodingException e) {
			Debug.rethrow(e);
		}
	}
	
	public NameMessage(String name,int ticket) {
		super(OPC_NAME, ticket, Util.getUTF8Bytes(name));
		this.stringValue=name;
	}

	

	public String getStringValue() {
		return stringValue;
	}
	
}
