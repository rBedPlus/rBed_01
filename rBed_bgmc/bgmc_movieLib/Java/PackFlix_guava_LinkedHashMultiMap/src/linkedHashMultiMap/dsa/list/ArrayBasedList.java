package linkedHashMultiMap.dsa.list;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Array Based List extends abstract list with generic type E
 * @author Eason Li
 *
 * @param <E> generic type
 */
public class ArrayBasedList<E> extends AbstractList<E> {

	/** default capacity */
	private final static int DEFAULT_CAPACITY = 10;
	/** array contains data */
	private E[] data;
	/** array size */
	private int size;

	/**
	 * ensure capacity function
	 * @param minCapacity minimum capacity
	 */
	private void ensureCapacity(int minCapacity) {
        int oldCapacity = data.length;
        if (minCapacity > oldCapacity) {
            int newCapacity = (oldCapacity * 2) + 1;
            if (newCapacity < minCapacity) {
                newCapacity = minCapacity;
            }
            data = Arrays.copyOf(data, newCapacity);
        }
    }
	
	/**
	 * default constructor with 10 capacity
	 */
	public ArrayBasedList() {
		this(DEFAULT_CAPACITY);
	}

	/**
	 * array list constructor
	 * @param capacity capacity
	 */
	@SuppressWarnings("unchecked")
	public ArrayBasedList(int capacity) {
		data = (E[]) (new Object[capacity]);
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
		ensureCapacity(size + 1);
		
		for (int i = size - 1; i >= index; i--) {
			data[i + 1] = data[i];
		}
		data[index] = value;
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
		
		return data[index];
	}

	/**
	 * remove function
	 * @param index position
	 * @return removed data in the array list
	 */
	@Override
	public E remove(int index) {
		checkIndex(index);
		E temp = data[index];
		
		for (int i = index; i < size - 1; i++) {
			data[i] = data[i + 1];
		}
		data[size - 1] = null;
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
		E oldItem = data[index];
		data[index] = value;
		return oldItem;
	}

	/**
	 * size function
	 * @return size of the array list
	 */
	@Override
	public int size() {
		
		return size;
	}

	/**
	 * iterator function
	 * @return new element iterator
	 */
	@Override
	public Iterator<E> iterator() {
		return new ElementIterator();
	}
	
	/**
	 * inner class element iterator
	 * @author Eason Li
	 *
	 */
	private class ElementIterator implements Iterator<E> {
		/** position */
	    private int position;
	    /** see if the element can remove or not */
	    private boolean removeOK;

	    /**
	     * Element iterator constructor
	     */
	    public ElementIterator() {
	        position = 0;
	        removeOK = false;
	    }

	    /**
	     * has next method
	     * @return true if the list has next element
	     */
	    public boolean hasNext() {
	    	
	        return position <= size() - 1;
	    }

	    /**
	     * next function
	     * @return next element in the list
	     */
	    public E next() {
	    	if(!hasNext()) throw new NoSuchElementException(); 
	    	position++;
	    	removeOK = true;
	    	return get(position - 1);
	        
	    }
	        
	    /**
	     * remove function
	     */
	    public void remove() {
	       if (removeOK) {
	    	   
	    	   ArrayBasedList.this.remove(position - 1);
	    	   position--;
	    	   removeOK = false; 
	       } else {
	    	   removeOK = false;
	    	   throw new IllegalStateException();
	       }
	       
	    }
	}
}