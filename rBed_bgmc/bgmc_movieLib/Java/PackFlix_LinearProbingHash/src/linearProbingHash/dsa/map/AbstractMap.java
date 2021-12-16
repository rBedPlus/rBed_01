package linearProbingHash.dsa.map;

import java.util.Iterator;

/**
 * Abstract map class
 * @author Eason Li
 *
 * @param <K> key
 * @param <V> value
 */
public abstract class AbstractMap<K, V> implements Map<K, V> {

	/**
	 * Map entry
	 * @author Eason Li
	 *
	 * @param <K> key
	 * @param <V> value
	 */
	protected static class MapEntry<K, V> implements Entry<K, V> {

		/**
		 * map key
		 */
		private K key;
		/**
		 * map value
		 */
		private V value;

		/**
		 * map entry
		 * @param key map key
		 * @param value map value
		 */
		public MapEntry(K key, V value) {
			setKey(key);
			setValue(value);
		}

		/**
		 * get key
		 * @return key
		 */
		@Override
		public K getKey() {
			return key;
		}

		/**
		 * get value
		 * @return value
		 */
		@Override
		public V getValue() {
			return value;
		}

		/**
		 * set key
		 * @param key map key
		 * @return new key
		 */
		public K setKey(K key) {
			this.key = key;
			return this.key;
		}

		/**
		 * set value
		 * @param value map value
		 * @return new value
		 */
		@Override
		public V setValue(V value) {
			V original = this.value;
			this.value = value;
			return original;
		}
	}

	/**
	 * key iterator
	 * @author Eason Li
	 *
	 */
	protected class KeyIterator implements Iterator<K> {
		
		/**
		 * iterator
		 */
		private Iterator<Entry<K, V>> it;
		
		/**
		 * key iterator constructor
		 * @param iterator iterator
		 */
		public KeyIterator(Iterator<Entry<K, V>> iterator) {
			this.it = iterator;
		}

		/**
		 * has next method
		 * @return true if has next
		 */
		@Override
		public boolean hasNext() {
			
			return it.hasNext();
		}

		/**
		 * next method
		 * @return next key
		 */
		@Override
		public K next() {
			
			return it.next().getKey();
		}
		
	}
	
	/**
	 * value iterator
	 * @author Eason Li
	 *
	 */
	protected class ValueIterator implements Iterator<V> {

		/**
		 * iterator
		 */
		private Iterator<Entry<K, V>> it;
		
		/**
		 * value iterator constructor
		 * @param iterator iterator
		 */
		public ValueIterator(Iterator<Entry<K, V>> iterator) {
			this.it = iterator;
		}

		/**
		 * has next method
		 * @return true if has next
		 */
		@Override
		public boolean hasNext() {
			
			return it.hasNext();
		}

		/**
		 * next method
		 * @return next key
		 */
		@Override
		public V next() {
			
			return it.next().getValue();
		}
		
	}

	/**
	 * is empty method
	 * @return true if list is empty
	 */
	@Override
	public boolean isEmpty() {
		return size() == 0;
	}
	
	/**
	 * iterator
	 * @return iterator with key
	 */
	@Override
	public Iterator<K> iterator() {
		return new KeyIterator(entrySet().iterator());
		
	}
	
	/**
	 * values with iterable
	 * @return value iterable
	 */
	@Override
	public Iterable<V> values() {
		return new ValueIterable();
	}
	
	/**
	 * ValueIterable
	 * @author Eason Li
	 *
	 */
	private class ValueIterable implements Iterable<V> {

		/**
		 * value Iterable
		 */
		private ValueIterable() {
			//empty
		}
		
		/**
		 * iterator
		 * @return iterator
		 */
		@Override
		public Iterator<V> iterator() {
			 return new ValueIterator(entrySet().iterator());
		}
	}
	
}