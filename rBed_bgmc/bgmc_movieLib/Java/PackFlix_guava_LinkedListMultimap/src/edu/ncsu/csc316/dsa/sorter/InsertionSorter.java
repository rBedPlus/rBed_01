package edu.ncsu.csc316.dsa.sorter;

import java.util.Comparator;

/**
 * InsertionSorter uses the insertion sort algorithm to sort data.
 * 
 * @author Dr. King
 * @author liyixin
 * 
 * @param <E> the generic type of data to sort
 */
public class InsertionSorter<E extends Comparable<E>> extends AbstractComparisonSorter<E> {

   //private Comparator<E> comparator;
    /**
     * insertion sorter
     */
    public InsertionSorter() {
        this(null);
    }
    
    /**
     * insertion sorter
     * @param comparator comparator
     */
    public InsertionSorter(Comparator<E> comparator) {
        //setComparator(comparator);
    	super(comparator);
    }
    
//    private void setComparator(Comparator<E> comparator) {
//        if(comparator == null) {
//           comparator = new NaturalOrder();
//        }
//        this.comparator = comparator;
//    }
    
	
	/**
	 * Sort method with parameter
	 * @param items items
	 */
	@Override
	public void sort(E[] items) {
	
		
		for (int i = 0; i <= items.length - 1; i++) {
			E x = items[i];
			int j = i - 1;
			
			while (j >= 0 && this.compare(items[j], x) > 0) {
				items[j + 1] = items[j];
				j = j - 1;
			}
			items[j + 1] = x;
		}
		
	}
	
//	private class NaturalOrder implements Comparator<E> {
//		
//		private NaturalOrder() {
//			//emtpy so far
//		}
//		
//	    public int compare(E first, E second) {
//	        return ((Comparable<E>) first).compareTo(second);
//	    }
//	}

	
}
