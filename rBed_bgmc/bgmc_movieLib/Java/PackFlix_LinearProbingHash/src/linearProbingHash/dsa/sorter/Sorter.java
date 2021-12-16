package linearProbingHash.dsa.sorter;

/**
 * Interface that defines the sorting behavior
 * @author Dr. King
 * @author liyixin
 * 
 * @param <E> the generic type of data to sort
 */
public interface Sorter<E> {
	
	/**
	 * sort method with parameter items
	 * @param items items
	 */
	void sort(E[] items);
}
