package linearProbingHash.dsa.map;

import java.util.Comparator;

/**
 * abstract sorted map class
 * @author Eason Li
 *
 * @param <K> key
 * @param <V> value
 */
public abstract class AbstractSortedMap<K extends Comparable<K>, V> extends AbstractMap<K, V> {

	/**
	 * comparator
	 */
	private Comparator<K> compare;

	/**
	 * abstract sorted map
	 * @param compare comparator
	 */
	public AbstractSortedMap(Comparator<K> compare) {
		if (compare == null) {
			this.compare = new NaturalOrder();
		} else {
			this.compare = compare;
		}
	}

	/**
	 * compare method
	 * @param key1 key one
	 * @param key2 key two
	 * @return compare number
	 */
	public int compare(K key1, K key2) {
		return compare.compare(key1, key2);
	}

	/**
	 * Natural order
	 * @author Eason Li
	 *
	 */
	private class NaturalOrder implements Comparator<K> {
		/**
		 * compare method
		 * @param first first object
		 * @param second second object
		 * @return compare number
		 */
		public int compare(K first, K second) {
			return ((Comparable<K>) first).compareTo(second);
		}
	}
}