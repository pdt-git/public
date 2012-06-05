package org.cs3.pl.cterm;

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
