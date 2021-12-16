package tree.movie.factory;

import net.datastructures.*;
import tree.dsa.list.ArrayBasedList;
import tree.dsa.list.List;
import tree.dsa.sorter.MergeSorter;
import tree.dsa.sorter.Sorter;

/**
 * Factory for creating new data structure and algorithm instances
 * 
 * @author Dr. King
 *
 */
public class DSAFactory {

	/**
	 * Returns a data structure that implements a map
	 * 
	 * @param <K>
	 *            - the key type
	 * @param <V>
	 *            - the value type
	 * @return a data structure that implements a map
	 */
	
	public static <K extends Comparable<K>, V> Map<K, V> getMap() {
		return getTreeMap();
	}

	/**
	 * Returns a data structure that implements an index-based list
	 * 
	 * @param <E>
	 *            - the element type
	 * @return an index-based list
	 */
	public static <E> List<E> getIndexedList() {
		return getArrayBasedList();
		
	}


	/**
	 * Returns a comparison based sorter
	 * 
	 * @param <E>
	 *            - the element type
	 * @return a comparison based sorter
	 */
	public static <E extends Comparable<E>> Sorter<E> getComparisonSorter() {
		return getMergeSorter();
	}




	private static <K extends Comparable<K>, V> TreeMap<K, V> getTreeMap() {
		return new TreeMap<K, V>();
	}

	
	
	/**
	 * Returns an array-based list
	 * 
	 * @return an array-based list
	 */
	private static <E> ArrayBasedList<E> getArrayBasedList() {
		return new ArrayBasedList<E>();
		
	}
	


	/**
	 * Returns a mergesorter
	 * 
	 * @return a mergesorter
	 */
	private static <E extends Comparable<E>> Sorter<E> getMergeSorter() {
		return new MergeSorter<E>();
	}

}