package linkedHashMultiMap.dsa.list;

/**
 * Interface list
 * @author Eason Li
 *
 * @param <E> generic type E
 */
public interface List<E> extends Iterable<E> {
	/**
	 * add method 
	 * @param index position
	 * @param value element that is added to the list
	 */
    void add(int index, E value);
    /**
     * add value to the front of the list
     * @param value element be added
     */
    void addFirst(E value);
    /**
     * add value to the back of the list
     * @param value element be added
     */
    void addLast(E value);
    /**
     * first element in the list
     * @return first element
     */
    E first();
    /**
     * get the element at index
     * @param index position in the list
     * @return get the element
     */
    E get(int index);
    /**
     * check if list is empty
     * @return true if list is empty
     */
    boolean isEmpty();
    /**
     * last element in the list
     * @return last element in the list
     */
    E last();
    /**
     * remove the element in the list
     * @param index position of the element
     * @return element removed
     */
    E remove(int index);
    /**
     * remove first element in the list
     * @return first element
     */
    E removeFirst();
    /**
     * remove last element in the list
     * @return last element
     */
    E removeLast();
    /**
     * set element in the list
     * @param index position of the list
     * @param value value of the element
     * @return the new element that set into list
     */
    E set(int index, E value);
    /**
     * size of the list
     * @return size
     */
    int size();
}