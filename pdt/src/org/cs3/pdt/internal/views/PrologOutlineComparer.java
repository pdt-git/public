package org.cs3.pdt.internal.views;

import org.cs3.pl.metadata.Predicate;
import org.eclipse.jface.viewers.IElementComparer;

final class PrologOutlineComparer implements IElementComparer {
	@Override
	public int hashCode(Object element) {
		if (element instanceof Predicate) {
			Predicate p = (Predicate) element;
			return p.getSignature().hashCode();
		}
		return element.hashCode();
	}

	@Override
	public boolean equals(Object a, Object b) {
		if (a instanceof Predicate && b instanceof Predicate) {
			Predicate pa = (Predicate) a;
			Predicate pb = (Predicate) b;
			return pa.getSignature().equals(pb.getSignature());
		}
		return a.equals(b);
	}
}