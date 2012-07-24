/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.cterm;

import java.util.Iterator;

import org.cs3.prolog.internal.cterm.parser.ASTAtom;
import org.cs3.prolog.internal.cterm.parser.ASTNode;

public class CCompound extends CTerm implements Iterable<CTerm> {
	private CTerm[] args;

	public CCompound(ASTNode node) {
		super(node);
		args = new CTerm[node.jjtGetNumChildren()-1]; 
	}

	public CTerm getArgument(int i) {
		if(args[i]==null){
			args[i]=CTermFactory.create(node.jjtGetChild(i+1));
		}
		return args[i];
	}

	protected String doGetFunctorImage() {
		return ((ASTAtom)node.jjtGetChild(0)).getString();
	}

	@Override
	public int getArity() {
		return args.length;
	}

	@Override
	public Iterator<CTerm> iterator() {
		for (int i = 0;i<args.length;i++) {
			getArgument(i);
		}
		return new ArrayIterator<CTerm>(args);
	}

}

