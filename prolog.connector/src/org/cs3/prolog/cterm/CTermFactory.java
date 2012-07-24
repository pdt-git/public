/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.cterm;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.internal.cterm.parser.ASTAtom;
import org.cs3.prolog.internal.cterm.parser.ASTCompound;
import org.cs3.prolog.internal.cterm.parser.ASTFloat;
import org.cs3.prolog.internal.cterm.parser.ASTInteger;
import org.cs3.prolog.internal.cterm.parser.ASTNil;
import org.cs3.prolog.internal.cterm.parser.ASTString;
import org.cs3.prolog.internal.cterm.parser.ASTVariable;
import org.cs3.prolog.internal.cterm.parser.CanonicalTermParser;
import org.cs3.prolog.internal.cterm.parser.Node;

public class CTermFactory {

	public static CTerm createCTerm(Object data) {
		CanonicalTermParser parser=null;
		if(data instanceof InputStream){
			parser = new CanonicalTermParser((InputStream) data); 
		}
		else if (data instanceof Reader){
			parser = new CanonicalTermParser((Reader) data);
		}
		else{
			String input = data.toString();
			Reader reader = new StringReader(input);
			parser = new CanonicalTermParser(reader);
		}
		try {
			parser.Start();
		} catch (Throwable e) {
			Debug.report(e);
			throw new RuntimeException(e.getMessage());
		}
		return create(parser.getASTRoot());
	}

	static CTerm create(Node root) {
		if(root instanceof ASTAtom){
			return new CAtom((ASTAtom)root);
		} 
		if(root instanceof ASTString){
			return new CString((ASTString)root);
		}
		if(root instanceof ASTVariable){
			return new CVariable((ASTVariable)root);
		} 
		if(root instanceof ASTCompound){
			return new CCompound((ASTCompound)root);
		} 
		if(root instanceof ASTInteger){
			return new CInteger((ASTInteger)root);
		}
		if(root instanceof ASTFloat){
			return new CFloat((ASTFloat)root);
		}
		if(root instanceof ASTNil){
			return new CNil((ASTNil)root);
		}
		throw new IllegalArgumentException("bad node type: "+root.getClass().getName());
	}

}

