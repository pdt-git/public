/*
 * tuProlog - Copyright (C) 2001-2006  aliCE team at deis.unibo.it
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package alice.tuprolog;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.HashMap;

/**
 * This class manages Prolog operators.
 *
 * @see Operator
 */
class OperatorManager implements java.io.Serializable {
	
	/** current known operators */
	private OperatorRegister operatorList = new OperatorRegister();
	
	/** lowest operator priority */
	public static final int OP_LOW = 1;
	
	/** highest operator priority */
	public static final int OP_HIGH = 1200;
	
	/**
	 * Creates a new operator. If the operator is already provided,
	 * it replaces it with the new one
	 */
	public void opNew(String name,String type,int prio) {
		final Operator op = new Operator(name, type, prio);
		if (prio >= OP_LOW && prio <= OP_HIGH)
			operatorList.addOperator(op);
	}
	
	/**
	 * Returns the priority of an operator (0 if the operator is not defined).
	 */
	public int opPrio(String name,String type) {
		Operator o = operatorList.getOperator(name, type);
		return (o == null) ? 0 : o.prio;
	}
	
	/**
	 * Returns the priority nearest (lower) to the priority of a defined operator
	 */
	public int opNext(int prio) {
		int n = 0;
		for (Iterator it = operatorList.iterator(); it.hasNext();) {
			Operator opFromList = (Operator) it.next();
			if (opFromList.prio > n && opFromList.prio < prio)
				n = opFromList.prio;
		}
		return n;
	}
	
	/**
	 *  Gets the list of the operators currently defined
	 *
	 *  @return the list of the operators
	 */
	public List getOperators() {
		return new LinkedList(operatorList);
	}
	
	/**
	 * Register for operators
	 * Cashes operator by name+type description.
	 * Retains insertion order as LinkedHashSet.
	 * <p/>
	 * todo Not 100% sure if 'insertion-order-priority' should be completely replaced
	 * by the explicit priority given to operators.
	 *
	 * @author ivar.orstavik@hist.no
	 */
	private static class OperatorRegister extends LinkedHashSet {
		//map of operators by name and type
		//key is the nameType of an operator (for example ":-xfx") - value is an Operator
		private HashMap nameTypeToKey = new HashMap();
		
		public boolean addOperator(Operator op) {
			final String nameTypeKey = op.name + op.type;
			Operator matchingOp = (Operator) nameTypeToKey.get(nameTypeKey);
			if (matchingOp != null)
				super.remove(matchingOp);       //removes found match from the main list
			nameTypeToKey.put(nameTypeKey, op); //writes over found match in nameTypeToKey map
			return super.add(op);               //adds new operator to the main list
		}
		
		public Operator getOperator(String name, String type) {
			return (Operator) nameTypeToKey.get(name + type);
		}
	}
	
}