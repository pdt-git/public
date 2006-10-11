/*
 * tuProlog - Copyright (C) 2001-2002  aliCE team at deis.unibo.it
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

/**
 * This class manages prolog operators
 *
 * @see Operator
 *
 */
public class OperatorManager implements java.io.Serializable {

    /** current known operators */
    protected alice.util.LinkedQueue     opQueue=new alice.util.LinkedQueue();

    /** lowest operator priority */
    public  static final int    OP_LOW        = 1;

    /** highest operator priority */
    public  static final int    OP_HIGH       = 1200;

    /**
     * Creates a new operator
     *
     * If the operator is already provided,
     * it replaces it with the new one
     *
     */
    public boolean opNew(String name,String type,int prio) {
        int a = opQueue.length();
        while(a-- > 0) {
            Operator o = (Operator)opQueue.remFirst();
            if(!o.name.equals(name) || !o.type.equals(type)){
                opQueue.insLast(o);
            }
        }
        if(prio >= OP_LOW && prio <= OP_HIGH){
            opQueue.insLast(new Operator(name,type,prio));
        }
        return(true);
    }

    /**
     * Gets the priority of an operator (OP_LOW if the operator is not defined)
     */
    public int opPrio(String name,String type) {
        int n = OP_LOW - 1;
        alice.util.LinkedList l = opQueue.head;
        while(!l.isEmptyList()) {
           Operator o = (Operator)l.head; l = l.tail;
           if(o.name.equals(name) && o.type.equals(type)) {
               n = o.prio;
               break;
           }
        }
        return(n);
    }

    /**
     * Gets the list of the operators currently defined
     */
    public  alice.util.LinkedList getOperators() {
        return opQueue.head;
    }

    /**
     * Gets the priority nearest (lower) to the priority of a defined operator
     */
    public int opNext(int prio) {
        int n = 0;
        alice.util.LinkedList l = opQueue.head;
        while(!l.isEmptyList()) {
            Operator o = (Operator)l.head; l = l.tail;
            if(o.prio > n  && o.prio < prio){
                n = o.prio;
            }
        }
        return(n);
    }
}