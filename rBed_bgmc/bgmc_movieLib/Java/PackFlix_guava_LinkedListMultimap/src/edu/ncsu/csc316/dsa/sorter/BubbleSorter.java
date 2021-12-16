package edu.ncsu.csc316.dsa.sorter;

import java.util.Comparator;

/**
 * Bubble sorter with generic E extends comparable and abstract comparison sorter
 * @author liyixin
 *
 * @param <E> the generic type of data to sort
 */
public class BubbleSorter<E extends Comparable<E>> extends AbstractComparisonSorter<E> {

	/**
	 * bubble sorter constructor
	 * @param comparator comparator
	 */
	public BubbleSorter(Comparator<E> comparator) {
		super(comparator);
	}

	/**
	 * bubble sorter
	 */
	public BubbleSorter() {
		this(null);
	}
	
	/**
	 * sort method
	 * @param items array items
	 */
	@Override
	public void sort(E[] items) {
		
		boolean r = true;
		while (r) {
			r = false;
			for (int i = 1; i <= items.length - 1; i++) {
				if (this.compare(items[i], items[i - 1]) < 0) {
					E x = items[i - 1];
					items[i - 1] = items[i];
					items[i] = x;
					r = true;
				}
			}
		}
	}

}
