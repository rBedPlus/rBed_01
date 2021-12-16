package edu.ncsu.csc316.dsa.sorter;

import java.util.Comparator;
import java.util.Random;

/**
 * quick sorter
 * @author Eason Li
 *
 * @param <E> generic type E
 */
public class QuickSorter<E extends Comparable<E>> extends AbstractComparisonSorter<E> {

	/**
	 * FIRST_ELEMENT_SELECTOR
	 */
    public static final PivotSelector FIRST_ELEMENT_SELECTOR = new FirstElementSelector();
    /**
     * LAST_ELEMENT_SELECTOR
     */
    public static final PivotSelector LAST_ELEMENT_SELECTOR = new LastElementSelector();
    /**
     * MIDDLE_ELEMENT_SELECTOR
     */
    public static final PivotSelector MIDDLE_ELEMENT_SELECTOR = new MiddleElementSelector();
    /**
     * RANDOM_ELEMENT_SELECTOR
     */
    public static final PivotSelector RANDOM_ELEMENT_SELECTOR = new RandomElementSelector();
    /**
     * Pivot Selector
     */
    private PivotSelector selector;
    
    /**
     * QuickSorter constructor
     * @param comparator comparator
     * @param selector selector
     */
    public QuickSorter(Comparator<E> comparator, PivotSelector selector) {
        super(comparator);
        setSelector(selector);
    }
    
    /**
     * QuickSorter constructor
     * @param comparator comparator
     */
	public QuickSorter(Comparator<E> comparator) {
		this(comparator, null);
	}    
    
	/**
     * QuickSorter constructor
     * @param selector selector
     */
    public QuickSorter(PivotSelector selector) {
        this(null, selector);
    }
    /**
     * QuickSorter constructor
     */
    public QuickSorter() {
        this(null, null);
    }
    
    /**
     * set selector
     * @param selector PivotSelector
     */
    private void setSelector(PivotSelector selector) {
        if(selector == null) {
            selector = new RandomElementSelector();
        }
        this.selector = selector;
    }
    
    /**
     * sort method
     * @param items array
     */
    @Override
	public void sort(E[] items) {
//    	for (int i = 0; i < items.length; i++) {
//			System.out.println(items[i]);
//		}
		quickSort(items, 0, items.length - 1);
	}
    
    /**
     * quick sort
     * @param arr array
     * @param low low index
     * @param high high index
     */
    private void quickSort(E[] arr, int low, int high) {
    	if (low < high) {
    		int pivotLocation = partition(arr, low, high);
    		quickSort(arr, low, pivotLocation - 1);
    		quickSort(arr, pivotLocation + 1, high);
    	}
    } 
    
    /**
     * partition method
     * @param arr array
     * @param low low index
     * @param high high index
     * @return index of partition
     */
    private int partition(E[] arr, int low, int high) {
    	
    	int pivotIndex = selector.selectPivot(low, high);
    	swap(arr, pivotIndex, high);
    	return partitionHelper(arr, low, high);
    }
    
    /**
     * partition helper
     * @param arr array
     * @param low low index
     * @param high high index
     * @return index of the data
     */
    private int partitionHelper(E[] arr, int low, int high) {
    	E pivot = arr[high];
    	int index = low;
    	for (int j = low; j <= high - 1; j++) {
    		if (super.compare(arr[j], pivot) <= 0) {
    			swap(arr, index, j);
    			index++;
    		}
    	}
    	swap(arr, index, high);
    	return index;
    }
    
    /**
     * swap method
     * @param arr array
     * @param low low index
     * @param high high index
     */
    private void swap(E[] arr, int low, int high) {
    	E lowtemp = arr[low];
    	E hightemp = arr[high];
    	arr[low] = hightemp;
    	arr[high] = lowtemp;
    	 
    }
    
    
    /**
     * PivotSelector interface
     * @author Eason Li
     *
     */
    private interface PivotSelector {
        /**
         * Returns the index of the selected pivot element
         * @param low - the lowest index to consider
         * @param high - the highest index to consider
         * @return the index of the selected pivot element
         */
        int selectPivot(int low, int high);
    }
    
    /**
     * First Element Selector
     * @author Eason Li
     *
     */
    public static class FirstElementSelector implements PivotSelector {

    	/**
    	 * empty constructor
    	 */
    	public FirstElementSelector() {
    		//do nothing
    	}
    	/**
         * Returns the index of the selected pivot element
         * @param low - the lowest index to consider
         * @param high - the highest index to consider
         * @return the index of the selected pivot element
         */
		@Override
		public int selectPivot(int low, int high) {
			return low;
		}
    	
    }
    
    /**
     * LastElementSelector class
     * @author Eason Li
     *
     */
    public static class LastElementSelector implements PivotSelector {
    	/**
    	 * empty constructor
    	 */
    	public LastElementSelector() {
    		//do nothing
    	}
    	/**
         * Returns the index of the selected pivot element
         * @param low - the lowest index to consider
         * @param high - the highest index to consider
         * @return the index of the selected pivot element
         */
		@Override
		public int selectPivot(int low, int high) {
			return high;
		}
    	
    }
    
    /**
     * MiddleElementSelector class
     * @author Eason Li
     *
     */
    public static class MiddleElementSelector implements PivotSelector {
    	/**
    	 * empty constructor
    	 */
    	public MiddleElementSelector() {
    		//do nothing
    	}
    	/**
         * Returns the index of the selected pivot element
         * @param low - the lowest index to consider
         * @param high - the highest index to consider
         * @return the index of the selected pivot element
         */
		@Override
		public int selectPivot(int low, int high) {
			// TODO Auto-generated method stub
			return (low + high) / 2;
		}
    	
    }
    
    /**
     * RandomElementSelector
     * @author Eason Li
     *
     */
    public static class RandomElementSelector implements PivotSelector {
    	/**
    	 * empty constructor
    	 */
    	public RandomElementSelector() {
    		//do nothing
    	}
    	/**
         * Returns the index of the selected pivot element
         * @param low - the lowest index to consider
         * @param high - the highest index to consider
         * @return the index of the selected pivot element
         */
		@Override
		public int selectPivot(int low, int high) {
			Random r = new Random();
			
			int result = r.nextInt(high - low) + low;
			return result;
		}
    	
    }
    

	
}