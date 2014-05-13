package pdt.y.utils;

import java.util.EventListener;

public interface GenericEventListener<T> extends EventListener {
	void valueChanged(T oldValue, T newValue);
}
