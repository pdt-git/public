/*
 * Created on 04.04.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.parser;

import java.util.Comparator;


public class NodePositionComparator implements Comparator{
		public int compare(Object o1, Object o2) {
			if (!(o1 instanceof SimpleNode) || !(o2 instanceof SimpleNode))
				throw new RuntimeException("incorrect types " + o1.getClass() + ", " + o2.getClass() +", expected SimpleNode.");
			Token token1 = ((SimpleNode)o1).getToken();
			Token token2 = ((SimpleNode)o2).getToken();
			if (token1.beginLine < token2.beginLine)
				return -1;
			if (token1.beginLine > token2.beginLine)
				return 1;
			if (token1.beginColumn < token2.beginColumn)
				return -1;
			if (token1.beginColumn > token2.beginColumn)
				return 1;
			return 0;
		}
		
		public boolean equals(Object o) {
			return compare(this, o) == 0;
		}
}
