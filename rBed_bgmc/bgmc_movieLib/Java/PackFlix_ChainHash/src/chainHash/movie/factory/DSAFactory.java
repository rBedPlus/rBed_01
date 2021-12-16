package chainHash.movie.factory;

import chainHash.dsa.list.ArrayBasedList;
import chainHash.dsa.list.List;
import chainHash.dsa.sorter.MergeSorter;
import chainHash.dsa.sorter.Sorter;
import net.datastructures.ChainHashMap;
import net.datastructures.Map;

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
    public static <K extends Comparable<K>, V> Map<K, V> getMap () {
        return getChainHashMap();
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

    /**
     * Returns a skip list map
     *
     * @return a skip list map
     */
    private static <K extends Comparable<K>, V> ChainHashMap<K, V> getChainHashMap () {
        return new ChainHashMap<K, V>();
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

}
