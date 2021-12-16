package hashMultiMap.movie.factory;

import com.google.common.collect.HashMultimap;

import hashMultiMap.dsa.list.ArrayBasedList;
import hashMultiMap.dsa.list.List;
import hashMultiMap.dsa.sorter.MergeSorter;
import hashMultiMap.dsa.sorter.Sorter;

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
    public static <K extends Comparable<K>, V> HashMultimap<K, V> getMap () {
        return getHashMultimap();
    }

    /**
     * Returns a data structure that implements an index-based list
     *
     * @param <E>
     *            - the element type
     * @return an index-based list
     */
    public static <E> List<E> getIndexedList () {
        return getArrayBasedList();

    }

    /**
     * Returns a comparison based sorter
     *
     * @param <E>
     *            - the element type
     * @return a comparison based sorter
     */
    public static <E extends Comparable<E>> Sorter<E> getComparisonSorter () {
        return getMergeSorter();
    }

    private static <K extends Comparable<K>, V> HashMultimap<K, V> getHashMultimap () {
        return HashMultimap.create();
    }

    /**
     * Returns an array-based list
     *
     * @return an array-based list
     */
    private static <E> ArrayBasedList<E> getArrayBasedList () {
        return new ArrayBasedList<E>();

    }

    /**
     * Returns a mergesorter
     *
     * @return a mergesorter
     */
    private static <E extends Comparable<E>> Sorter<E> getMergeSorter () {
        return new MergeSorter<E>();
    }

    /**
     * Returns a quicksorter
     *
     * @return a quicksorter
     */

}
