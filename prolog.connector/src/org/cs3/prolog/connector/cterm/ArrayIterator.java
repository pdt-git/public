/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Tobias Rho, Lukas Degener, Andreas Becker, Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.connector.cterm;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * An object that iterates over the elements of an array
 */
public class ArrayIterator<T> implements Iterator<T> {
	T[] elements;
	int index;
	int lastElement;

	public ArrayIterator(T[] elements) {
		this(elements, 0, elements.length - 1);
	}

	public ArrayIterator(T[] elements, int firstElement, int lastElement) {
		super();
		this.elements = elements;
		index = firstElement;
		this.lastElement = lastElement;
	}

	@Override
	public boolean hasNext() {
		return elements != null && index <= lastElement;
	}

	@Override
	public T next() throws NoSuchElementException {
		if (!hasNext())
			throw new NoSuchElementException();
		return elements[index++];
	}

	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}
}


