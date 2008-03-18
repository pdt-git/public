package org.cs3.pifcom.codec;

import java.nio.charset.Charset;

import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.PLUtil;

public class CTermMessage extends Message {

	public CTermMessage(int opc, int ticket, String query){
		super(opc,ticket,query.getBytes(Charset.forName("UTF-8")));
	}

	public CTermMessage(int opc, int ticket, byte[] body) {
		super(opc, ticket, body);

	}
	
	public String getStringValue(){
		String string = new String(this.getBody(),Charset.forName("UTF-8"));
		if(string.endsWith(".")){
			return string.substring(0, string.lastIndexOf('.'));	
		}
		else{
			return string;
		}
		
	}
	
	public CTerm getCTermValue(){
		return PLUtil.createCTerm(getStringValue());
	}

}
