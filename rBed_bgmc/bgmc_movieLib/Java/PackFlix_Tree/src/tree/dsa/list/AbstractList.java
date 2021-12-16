package tree.dsa.list;

/**
 * Abstract class abstract list
 * @author Eason Li
 *
 * @param <E> generic type E
 */
public abstract class AbstractList<E> implements List<E> {
	
	/**
	 * add value at the first position
	 * @param value add to the front
	 */
	@Override
	public void addFirst(E value) {
		add(0, value);
	}
	
	/**
	 * add value at the last position
	 * @param value add to the back
	 */
	@Override
	public void addLast(E value) {
		add(size(), value);
	}

	/**
	 * check index method
	 * @param index input number
	 * @throws IndexOutOfBoundsException IndexOutOfBoundsException
	 */
	protected void checkIndex(int index)
	{
		if(index < 0 || index >= size())
		{
			throw new IndexOutOfBoundsException("Index is invalid: " + index + " (size=" + size() + ")");
		}
	}
	
	/**
	 * check index for add method
	 * @param index input number
	 * @throws IndexOutOfBoundsException IndexOutOfBoundsException
	 */
	protected void checkIndexForAdd(int index)
	{
		if(index < 0 || index > size())
		{
			throw new IndexOutOfBoundsException("Index is invalid: " + index + " (size=" + size() + ")");
		}
	}
	
	/**
	 * first element in the list
	 * @return first element
	 */
	@Override
	public E first() {
		return get(0);
	}
	
	/**
	 * check if list is empty or not
	 * @return true if list is empty
	 */
	@Override
	public boolean isEmpty() {
		return size() == 0;
	}

	/**
	 * last element in the list
	 * @return last element
	 */
	@Override
	public E last() {
		return get(size() - 1);
	}
	
	/**
	 * remove first element in the list
	 * @return the first element that is removed
	 */
	@Override
	public E removeFirst() {
		return remove(0);
	}
	
	/**
	 * remove last element
	 * @return the last element that is removed
	 */
	@Override
	public E removeLast() {
		return remove(size() - 1);
	}
}