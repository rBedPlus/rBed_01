package linearProbingHash.dsa.map;

import java.util.Iterator;

/**
 * Map interface
 * @author Eason Li
 *
 * @param <K> key
 * @param <V> value
 */
public interface Map<K, V> extends Iterable<K> {
	/**
	 * entrySet
	 * @return iterable entry
	 */
	Iterable<Entry<K, V>> entrySet();
	
	/**
	 * get method
	 * @param key map key
	 * @return value
	 */
	V get(K key);
	
	/**
	 * is empty method
	 * @return true if list is empty
	 */
	boolean isEmpty();
	
	/**
	 * iterator
	 * @return iterator
	 * 
	 */
	Iterator<K> iterator();
	
	/**
	 * put method
	 * @param key map key
	 * @param value map value
	 * @return value
	 */
	V put(K key, V value);
	
	/**
	 * remove method
	 * @param key map key
	 * @return value
	 */
	V remove(K key);
	
	/**
	 * size method
	 * @return list size
	 */
	int size();
	
	/**
	 * values method
	 * @return iterable
	 */
	Iterable<V> values();
	
	/**
	 * entry interface
	 * @author Eason Li
	 *
	 * @param <K> key
	 * @param <V> value
	 */
	interface Entry<K, V> {
		/**
		 * get key method
		 * @return key
		 */
		K getKey();
		
		/**
		 * get value method
		 * @return value
		 */
		V getValue();
		
		/**
		 * set key 
		 * @param key new key
		 * @return key
		 */
		K setKey(K key);
		
		/**
		 * set value
		 * @param value value
		 * @return new value
		 */
		V setValue(V value);
	}
}