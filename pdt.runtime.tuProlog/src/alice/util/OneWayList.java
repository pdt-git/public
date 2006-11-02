package alice.util;

import java.util.*;

public class OneWayList {
	
	private Object head;
	private OneWayList tail;
	
	public OneWayList(Object head, OneWayList tail){
		this.head = head;
		this.tail = tail;
	}

	public static OneWayList transform(List list){
		if(list.isEmpty()) return null;
		return new OneWayList(list.remove(0),transform(list));
	}
	
	
	public Object getHead() {
		return head;
	}
	
	public void setHead(Object head) {
		this.head = head;
	}


	public OneWayList getTail() {
		return tail;
	}
	
	public void setTail(OneWayList tail) {
		this.tail = tail;
	}

	public void addLast(OneWayList newTail){
		if(tail == null){
			tail = newTail;
			return;
		}
		tail.addLast(newTail);
	}
	
	public OneWayList get(int index){
		if(tail == null) throw new NoSuchElementException();
		if(index <= 0) return this;
		return tail.get(index-1);
	}

	public String toString() {
		String elem;
		if(head==null) elem = "null";
			else elem = head.toString();
		if(tail==null) return "["+elem+"]";
		return "["+tail.toString(elem)+"]";
	}
	
	private String toString(String elems){
		String elem;
		if(head==null) elem = "null";
			else elem = head.toString();
		if(tail==null) return elems+","+elem;
		return elems+","+tail.toString(elem);
	}
	
}