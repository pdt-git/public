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
 * This class represents an iterator through the arguments of a Struct list.
 *
 * @see Struct
 *
 *
 *
 */
class StructIterator implements java.util.Iterator, java.io.Serializable {

    Struct t;

    StructIterator(Struct t){
        this.t=t;
    }

    public boolean hasNext(){
        return !t.isEmptyListRaw();
    }

    public Object next(){
        Term co=t.getTerm(0);
        t=(Struct)(t.getTerm(1));
        return co;
    }

    public void remove(){
    }
}