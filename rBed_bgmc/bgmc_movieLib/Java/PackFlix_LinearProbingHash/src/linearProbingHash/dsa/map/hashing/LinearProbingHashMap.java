package linearProbingHash.dsa.map.hashing;

import linearProbingHash.dsa.list.ArrayBasedList;

/**
 * LinearProbingHashMap class
 * @author Eason Li
 *
 * @param <K> key
 * @param <V> value
 */
public class LinearProbingHashMap<K, V> extends AbstractHashMap<K, V> {

    // This time, our array is an array of TableEntry objects
	/**
	 * table
	 */
    private TableEntry<K, V>[] table;
    /**
     * size
     */
    private int size;

    /**
     * constructor
     */
    public LinearProbingHashMap() {
        this(AbstractHashMap.DEFAULT_CAPACITY, false);
    }
    
    /**
     * constructor
     * @param isTesting boolean type
     */
    public LinearProbingHashMap(boolean isTesting) {
        this(AbstractHashMap.DEFAULT_CAPACITY, isTesting);
    }
    
    /**
     * constructor
     * @param capacity capacity
     */
    public LinearProbingHashMap(int capacity) {
        this(capacity, false);
    }
    
    /**
     * constructor
     * @param capacity capacity
     * @param isTesting boolean type
     */
    public LinearProbingHashMap(int capacity, boolean isTesting) {
        super(capacity, isTesting);
        size = 0;
    }

    /**
     * entry set
     * @return array list
     */
    @Override
    public Iterable<Entry<K, V>> entrySet() {
        ArrayBasedList<Entry<K, V>> buffer = new ArrayBasedList<>();
        for (int i = 0; i < capacity(); i++) {
        	if (!isAvailable(i)) {
        		buffer.addLast(table[i]);
        	}
        }
        return buffer;
    }

    /**
     * create table
     * @param capacity capacity of table
     */
    @SuppressWarnings("unchecked")
    @Override
    public void createTable(int capacity) {
        table = (TableEntry<K, V>[]) new TableEntry[capacity];
        size = 0;
    }
    
    /**
     * Helper method to determine whether a bucket has an entry or not  
     * @param index index
     * @return true if is available
     */
    private boolean isAvailable(int index) {
        return (table[index] == null || table[index].isDeleted());
    }

    /**
     *  Helper method to find the bucket for an entry;
     *  If the entry *is* in the map, returns the index of the bucket
     *  If the entry is *not* in the map, returns -(a + 1) to indicate 
     *  that the entry should be added at index a
     * @param index index 
     * @param key key
     * @return integer
     */
    // 
    private int findBucket(int index, K key) {
        int avail = -1;
        int j = index;
        do {
        	if (isAvailable(j)) {
        		if (avail == -1) {
        			avail = j;
        		}
        		if (table[j] == null) {
        			return -(avail + 1);
        		}
        	} else if (table[j].getKey().equals(key)) {
        		return j;
        	}
        	j = (j + 1) % table.length;
        } while (j != index);
        return -(avail + 1);
    }
    
    /**
     * bucket get method
     * @param hash hash code
     * @param key key
     */
    @Override
    public V bucketGet(int hash, K key) {
        int index = findBucket(hash, key);
       if (index < 0) {
    	   return null;
       }
       return table[index].getValue();
    }

    /**
     * bucket put
     * @param hash hash code
     * @param key key
     * @param value value
     * @return value
     */
    @Override
    public V bucketPut(int hash, K key, V value) {
        int index = findBucket(hash, key);
        if (index >= 0) {
        	return table[index].setValue(value);
        }
        table[-(index + 1)] = new TableEntry<>(key, value);
        size++;
        return null;
    }   

    /**
     * bucket remove
     * @param hash hash code
     * @param key key
     * @return value
     */
    @Override
    public V bucketRemove(int hash, K key) {
        int index = findBucket(hash, key);
        if (index < 0) {
        	return null;
        }
        V result = table[index].getValue();
        table[index] = null;
        size--;
        return result;
    }
    
    /**
     * size
     * @return size
     */
    @Override
    public int size() {
        return size;
    }

    /**
     * capacity
     * @return capacity
     */
    @Override
    protected int capacity() {
        return table.length;
    }
    
    /**
     * table entry
     * @author Eason Li
     *
     * @param <K> key
     * @param <V> value
     */
    private static class TableEntry<K, V> extends MapEntry<K, V> {

    	/**
    	 * true if is deleted
    	 */
        private boolean isDeleted;

        /**
         * table entry
         * @param key key
         * @param value value
         */
        public TableEntry(K key, V value) {
            super(key, value);
            setDeleted(false);
        }

        /**
         * is deleted method
         * @return true if is deleted
         */
        public boolean isDeleted() {
            return isDeleted;
        }

        /**
         * set deleted
         * @param deleted true if is deleted
         */
        public void setDeleted(boolean deleted) {
            isDeleted = deleted;
        }
    }
}