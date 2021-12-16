package linearProbingHash.dsa.sorter;

import java.util.Comparator;

/**
 * abstract class extends comparable and implements sorter
 * @author liyixin
 *
 * @param <E> the generic type of data to sort
 */
public abstract class AbstractComparisonSorter<E extends Comparable<E>> implements Sorter<E> {

	/**
	 * comparator field
	 */
    private Comparator<E> comparator;
    
    /**
     * constructor with parameter comparator
     * @param comparator comparator
     */
    public AbstractComparisonSorter(Comparator<E> comparator) {
        setComparator(comparator);
    }
    
    /**
     * private method to set comparator
     * @param comparator comparator
     */
    private void setComparator(Comparator<E> comparator) {
        if(comparator == null) {
           comparator = new NaturalOrder();
        }
        this.comparator = comparator;
    }     
    
    /**
     * Natural order inner class implements comparator
     * @author liyixin
     *
     */
    private class NaturalOrder implements Comparator<E> {
    	
    	/**
    	 * natural order constructor
    	 */
    	private NaturalOrder() {
    		//nothing so far
    	}
    	
    	/**
    	 * compare method to check if two are equal
    	 * @param first 1st obj
    	 * @param second 2nd obj
    	 * @return 0 if two objects are equal.
    	 */
        public int compare(E first, E second) {
            return ((Comparable<E>) first).compareTo(second);
        }
    }
    
    /**
     * compare method
     * @param data1 data 1
     * @param data2 data 2
     * @return 0 if 1 and 2 are equal
     */
    public int compare(E data1, E data2) {
        return comparator.compare(data1,  data2);
    }
}
