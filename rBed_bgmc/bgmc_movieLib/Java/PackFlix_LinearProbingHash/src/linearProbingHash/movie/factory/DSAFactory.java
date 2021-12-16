package linearProbingHash.movie.factory;

import linearProbingHash.dsa.list.ArrayBasedList;
import linearProbingHash.dsa.list.List;
import linearProbingHash.dsa.map.Map;
import linearProbingHash.dsa.map.hashing.LinearProbingHashMap;
// import edu.ncsu.csc316.dsa.map.SearchTableMap;
// import edu.ncsu.csc316.dsa.map.search_tree.SplayTreeMap;
import linearProbingHash.dsa.sorter.MergeSorter;
import linearProbingHash.dsa.sorter.Sorter;

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
        return getLinearProbingHashMap();
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

    private static <K extends Comparable<K>, V> LinearProbingHashMap<K, V> getLinearProbingHashMap () {
        return new LinearProbingHashMap<K, V>();
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
