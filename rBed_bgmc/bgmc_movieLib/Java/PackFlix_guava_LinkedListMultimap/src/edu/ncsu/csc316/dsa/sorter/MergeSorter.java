package edu.ncsu.csc316.dsa.sorter;

import java.util.Arrays;
import java.util.Comparator;

/**
 * Merge Sorter
 * @author Eason Li
 *
 * @param <E> generic type E
 */
public class MergeSorter<E extends Comparable<E>> extends AbstractComparisonSorter<E> {

	/**
	 * Merge sorter constructor
	 * @param comparator comparator
	 */
	public MergeSorter(Comparator<E> comparator) {
		super(comparator);
	}
	
	/**
	 * Merge sorter constructor
	 */
	public MergeSorter() {
		this(null);
	}
 
	/**
	 * Sort method
	 * @param items array items
	 */
	@Override
	public void sort(E[] items) {
		
//		for (int i = 0; i < items.length; i++) {
//			System.out.println(items[i]);
//		} 
		
		int n = items.length;
		if (n < 2) {
			return;
			
		} 			
		int mid = n / 2;
		E[] left = Arrays.copyOfRange(items, 0, mid);
		E[] right = Arrays.copyOfRange(items, mid, n);
		
		
		sort(left);
		sort(right);
		merge(left, right, items);
		
	}
	
	/**
	 * merge method
	 * @param left left array
	 * @param right right array
	 * @param items items array
	 */
	private void merge(E[] left, E[] right, E[] items) {
		int leftIndex = 0;
		int rightIndex = 0;
	
		while (leftIndex + rightIndex < items.length) {
			if (rightIndex == right.length || (leftIndex < left.length && super.compare(left[leftIndex], right[rightIndex]) < 0)) {
				items[leftIndex + rightIndex] = left[leftIndex];
				leftIndex += 1;
			} else {
				items[leftIndex + rightIndex] = right[rightIndex];
				rightIndex += 1;
			}
		}
	}

}