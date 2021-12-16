package edu.ncsu.csc316.dsa.sorter;

import java.util.Comparator;


/**
 * SelectionSorter uses the selection sort algorithm to sort data
 * @author Dr. King
 * @author liyixin
 *
 * @param <E> the generic type of data to sort
 */
public class SelectionSorter<E extends Comparable<E>> extends AbstractComparisonSorter<E> {

	/**
	 * selection sorter
	 * @param comparator comparator
	 */
    public SelectionSorter(Comparator<E> comparator) {
        //setComparator(comparator);
    	super(comparator);
    } 
    
    /**
     * selection sorter
     */
    public SelectionSorter() {
    	this(null);
    }
    
    /**
	 * sort method
	 * @param data array data
	 */
    public void sort(E[] data) {
        
    	for (int i = 0; i <= data.length - 1; i++) {
    		int min = i;
    		for (int j = i + 1; j <= data.length - 1; j++) {
    			if (this.compare(data[j], data[min]) < 0) {
    				min = j;
    			}
    		}
    		if (i != min) {
    			E x = data[i];
    			data[i] = data[min];
    			data[min] = x;
    		}
    	}
    }
 
}