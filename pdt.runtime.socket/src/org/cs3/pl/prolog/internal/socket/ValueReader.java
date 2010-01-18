package org.cs3.pl.prolog.internal.socket;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Stack;
import java.util.Vector;

import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CAtom;
import org.cs3.pl.cterm.CString;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CTermFactory;
import org.cs3.pl.cterm.CVariable;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;

public class ValueReader {
	private static final char END_OF_VALUE_CHAR = '>';
	private static final char BEGIN_OF_VALUE_CHAR = '<';
	static final char END_OF_LIST_CHAR = '}';
	static final char BEGIN_OF_LIST_CHAR = '{';
	static final int EOF_CHAR = -1;
	private SocketClient socketClient;
	private BufferedReader reader;
	private boolean hasToContinue;
	private StringBuffer valueBuffer;
	private Stack<Vector<Object>> stack;
	
	ValueReader(SocketClient socketClient) {
		this.socketClient=socketClient;
	}

	public  Object readValue(int flags) throws IOException {
		Object value = null;
		value = doReading(flags);
		return value;
	}

	private Object doReading(int flags)
			throws IOException {
		Object value=null;
		prepeareEverythingForNextReading();
		char lastReadCharacter = readFirstInterestingCharacter(reader);
		while (hasToContinue) {
			value = analyseCharacter(flags,lastReadCharacter);
			if (hasToContinue) {
				lastReadCharacter = (char)reader.read();
			}
		}
		checkForUnexpectedEOF(lastReadCharacter);
		return value;
	}

	private void prepeareEverythingForNextReading() {
		reader = socketClient.getReader();
		valueBuffer = new StringBuffer();
		stack = new Stack<Vector<Object>>();
		hasToContinue=true;
	}

	private Object analyseCharacter(int flags, char lastReadCharacter) throws IOException {
		Object value = null;
		switch (lastReadCharacter) {
		case BEGIN_OF_VALUE_CHAR:
			clearValueBuffer();
			break;
		case BEGIN_OF_LIST_CHAR:
			stack.push(new Vector<Object>());
			break;
		case  END_OF_VALUE_CHAR:
			value = parseValue(flags);
			appendToTopmostListIfStackIsNotEmpty(value);
			break;
		case END_OF_LIST_CHAR:
			value = popTopmostList();
			appendToTopmostListIfStackIsNotEmpty(value);
			break;
		case (char)EOF_CHAR:
			hasToContinue=false;
			break;
		default:
			valueBuffer.append(lastReadCharacter);
		}
		return value;
	}

	private Object popTopmostList()
			throws IOException {
		Object value;
		if (stack.isEmpty()) {
			throw new IOException(
					"Read a closing curly bracket ('}') but there is no containing list!");
		}
		value = stack.pop();
		return value;
	}

	private Object parseValue(int flags) {
		Object value;
		String unparsedValue = Util.unescapeBuffer(valueBuffer);
		CTerm ctermValue = CTermFactory.createCTerm(unparsedValue);
		
		if (Util.flagsSet(flags,PrologInterface.CTERMS)) {
			value=ctermValue;
		} else{
			if(Util.flagsSet(flags, PrologInterface.UNQUOTE_ATOMS)					
				&&(ctermValue instanceof CString ||ctermValue instanceof CAtom)){
			value = ctermValue.getFunctorValue();
			}
			else if (ctermValue instanceof CVariable) {
				value=((CVariable)ctermValue).getFunctorValue();
			}
			else{
				value=PLUtil.renderTerm(ctermValue);
			}
		}
		return value;
	}

	private boolean invalidFirstCharacter(int firstCharacter) {
		return !(firstCharacter == BEGIN_OF_VALUE_CHAR || firstCharacter == BEGIN_OF_LIST_CHAR);
	}

	private char readFirstInterestingCharacter(BufferedReader reader)
	throws IOException {
		char firstInterestingCharacter = skipWhiteSpace(reader);
		checkForUnexpectedEOF((int)firstInterestingCharacter);
		if (invalidFirstCharacter(firstInterestingCharacter)) {
			reader.reset();
			hasToContinue=false;
		}
		return firstInterestingCharacter;
	}

	private void checkForUnexpectedEOF(int lastReadCharacter)
			throws IOException {
		if (lastReadCharacter == EOF_CHAR) {
			throw new IOException(
			"read EOF, while skipping whitespace before value.");
		}
	}

	private char skipWhiteSpace(BufferedReader reader) throws IOException {
		reader.mark(1);
		int readCharacter = reader.read();
		while (readCharacter != EOF_CHAR && Character.isWhitespace((char) readCharacter)) {
			reader.mark(1);
			readCharacter = reader.read();
		}
		return (char)readCharacter;
	}

	private void clearValueBuffer() {
		valueBuffer.setLength(0);
	}

	private void appendToTopmostListIfStackIsNotEmpty(Object value) {
		// if the stack is empty, we are finished.
		// Otherwise, the value becomes element of the list lying on top of
		// the stack.
		if (stack.isEmpty()) {
			hasToContinue=false;
		} else {
			Vector<Object> l = stack.peek();
			l.add(value);
		}
	}

}
