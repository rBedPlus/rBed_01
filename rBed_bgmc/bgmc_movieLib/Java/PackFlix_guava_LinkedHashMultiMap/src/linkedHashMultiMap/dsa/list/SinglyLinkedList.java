package linkedHashMultiMap.dsa.list;

import java.util.Iterator;
import java.util.NoSuchElementException;
/**
 * Singly linked list
 * @author Eason Li
 *
 * @param <E> generic type E
 */
public class SinglyLinkedList<E> extends AbstractList<E> {

	/** front node */
	private LinkedListNode<E> front;
	/** tail node */
	private LinkedListNode<E> tail;
	/** size of the list */
	private int size;
		
	/**
	 * Singly linked list constructor
	 */
	public SinglyLinkedList() {
		front = new LinkedListNode<E>(null);
		tail = null;
		size = 0;
	}

	/**
	 * add function that adds value to index position
	 * @param index position
	 * @param value E value
	 */
	@Override
	public void add(int index, E value) {
		checkIndexForAdd(index);
		
		if (isEmpty()) {
			LinkedListNode<E> temp = new LinkedListNode<E>(value);
			front.setNext(temp);
			tail = temp;
		} else {
			if (index == 0) {
				LinkedListNode<E> temp = new LinkedListNode<E>(value, front.getNext());
				front.setNext(temp);
			} else {
				LinkedListNode<E> current = front.getNext();
				for (int i = 0; i < index - 1; i++) {
					current = current.getNext();
				}
				current.setNext(new LinkedListNode<E>(value, current.next));
			}
		}
		size++;
	}

	/**
	 * get function
	 * @param index position
	 * @return data at index
	 */
	@Override
	public E get(int index) {
		checkIndex(index);
		
		LinkedListNode<E> current = front.getNext();
		for (int i = 0; i < index; i++) {
			current = current.getNext();
		}
		return current.getElement();
	}

	/**
	 * remove function
	 * @param index position
	 * @return removed data in the array list
	 */
	@Override
	public E remove(int index) {
		checkIndex(index);
		E temp;
		if (isEmpty()) {
			return null;
		} else {
			if (index == 0) {
				if (size == 1) {
					temp = front.getNext().getElement();
					front.setNext(null);
					tail = null;
				} else {
					temp = front.getNext().getElement();
					front.setNext(front.getNext().getNext());
				}
			} else {
				LinkedListNode<E> current = front.getNext();
				for (int i = 0; i < index - 1; i++) {
					current = current.getNext();
				}
				temp = current.getNext().getElement();
				current.setNext(current.getNext().getNext());
				if (index == size - 1) {
					tail = current.getNext();
				}
			}
		}
		size--;
		return temp;
	}

	/**
	 * set function
	 * @param index position
	 * @param value E value
	 */
	@Override
	public E set(int index, E value) {
		checkIndex(index);
		LinkedListNode<E> current = front.getNext();
		for(int i = 0; i < index; i++) {
			current = current.getNext();
		}
		E temp = current.getElement();
		current.setElement(value);
		return temp;
	}

	/**
	 * size function
	 * @return size of the array list
	 */
	@Override
	public int size() {
		
		return this.size;
	}
	
	/**
	 * last element in the list
	 * @return last element
	 */
	@Override
    public E last() {
        return tail.getElement();
    }
	
	
	/**
	 * add value at the last position
	 * @param value add to the back
	 */
    @Override
    public void addLast(E value) {
    	LinkedListNode<E> temp = new LinkedListNode<E>(value, null);
		if (isEmpty()) {
			front.setNext(temp);
		} else {
			tail.setNext(temp);
		}
 
		tail = temp;
		size++;
    }

    /**
	 * iterator function
	 * @return new element iterator
	 */
	@Override
	public Iterator<E> iterator() {
		// We need to tell the iterator to skip the dummy/sentinel node
	    return new ElementIterator(front.getNext());
	}
	
	/**
	 * linked list node
	 * @author Eason Li
	 *
	 * @param <E> generic type E
	 */
	private static class LinkedListNode<E> {
        /** data */
        private E data;
        /** next node */
        private LinkedListNode<E> next;
        
        /**
         * linked list node constructor
         * @param e element e
         */
        public LinkedListNode(E e) {
        	data = e;
        	next = null;
        }
        
        /**
         * linked list node constructor
         * @param e element e
         */
        public LinkedListNode(E e, LinkedListNode<E> node) {
        	data = e;
        	next = node;
        }
        
        /**
         * get next node
         * @return next node
         */
        public LinkedListNode<E> getNext() {
        	return next;
        }
        
        /**
         * get element of the node
         * @return value of the node
         */
        public E getElement() {
        	return data;
        }
        
        /**
         * set next node
         * @param e next node
         */
        public void setNext(LinkedListNode<E> e) {
        	this.next = e;
        }
        
        /**
         * set node element 
         * @param e element
         */
        public void setElement(E e) {
        	this.data = e;
        }
    }
	
	/**
	 * Element iterator
	 * @author Eason Li
	 *
	 */
	private class ElementIterator implements Iterator<E> {
	    // Keep track of the next node that will be processed
	    /**
	     * current node
	     */
		private LinkedListNode<E> current;
	    // Keep track of the node that was processed on the last call to 'next'
	   /**
	    * previous node
	    */
		private LinkedListNode<E> previous;
	    // Keep track of the previous-previous node that was processed
	    // so that we can update 'next' links when removing
	    /**
	     * previous previous node
	     */
		private LinkedListNode<E> previousPrevious;
	    /** check if it can be removed */
		private boolean removeOK;

		/**
		 * element iterator constructor
		 * @param start start node
		 */
	    public ElementIterator(LinkedListNode<E> start) {
	        current = start;
	        previous = null;
	        previousPrevious = null;
	        removeOK = false;
	    }

	    /**
	     * check if has next node
	     * @return true if has next node
	     */
	    public boolean hasNext() {
	        return current != null;
	    }

	    /**
	     * next function that return the next node element
	     * @return next node
	     */
	    public E next() {
	        if (!hasNext()) {
	        	throw new NoSuchElementException();
	        }
	        removeOK = true;
	        previousPrevious = previous;
	    	previous = current;
	    	E temp = current.getElement();
	    	current = current.getNext();
	    	
	        return temp; 
	    }
	      
	    /**
	     * remove method
	     */
	    public void remove() {
	    	if(removeOK) {	        	
	        	LinkedListNode<E> before = previous;
	        	
	        	LinkedListNode<E> here = front.getNext();
	        	int count = 0;
	        	while(here != before) {
	        		count++;
	        		here = here.getNext();
	        	}
	        	previous = previousPrevious;
	        	SinglyLinkedList.this.remove(count);
	        	removeOK = false; 
	        } else {
	        	throw new IllegalStateException();
		    }
	    }
	}
	
}
