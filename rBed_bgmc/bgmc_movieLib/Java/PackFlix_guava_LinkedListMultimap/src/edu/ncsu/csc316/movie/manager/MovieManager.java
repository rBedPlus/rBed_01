package edu.ncsu.csc316.movie.manager;

import java.io.FileNotFoundException;
import java.text.ParseException;

import com.google.common.collect.*;

import edu.ncsu.csc316.dsa.list.*;
import edu.ncsu.csc316.dsa.sorter.Sorter;
import edu.ncsu.csc316.movie.data.Movie;
import edu.ncsu.csc316.movie.data.WatchRecord;
import edu.ncsu.csc316.movie.factory.*;
import edu.ncsu.csc316.movie.io.*;

/**
 * Movie Manager
 * @author Eason Li
 *
 */
public class MovieManager {

	/**
	 * movie list
	 */
	private List<Movie> movielist;
	/**
	 * watch list
	 */
	private List<WatchRecord> watchlist;
	/**
	 * Creates a MovieManager instance for handling utility functions
	 * 
	 * @param pathToMovies  the path to the file of movie records
	 * @param pathToHistory the path to the file of watch records
	 * @throws FileNotFoundException if the file cannot be found
	 * @throws ParseException        if the watch history file has incorrectly
	 *                               formatted date information
	 */
	public MovieManager(String pathToMovies, String pathToHistory) throws FileNotFoundException, ParseException {
		movielist = InputFileReader.readMovieFile(pathToMovies);
		watchlist = InputFileReader.readHistoryFile(pathToHistory);
	}
   
	/**
	 * Returns a list of watch records associated with the requested movie title
	 * 
	 * @param title the title of the movie for which to retrieve watch record
	 *              information
	 * @return a list of watch records associated with the requested movie title
	 */
	public List<WatchRecord> getWatchHistory(String title) {
		
		String tempID = null;
		for (int i = 0; i < movielist.size(); i++) {
			if (movielist.get(i).getTitle().equals(title)) {
				tempID = movielist.get(i).getId();
				break;
			}
		}
		if (tempID == null) {
			return DSAFactory.getIndexedList();
		}
		List<WatchRecord> list = DSAFactory.getIndexedList();
		for (int i = 0; i < watchlist.size(); i++) {
			if (watchlist.get(i).getMovieId().equals(tempID)) {
				list.addLast(watchlist.get(i));
			}
		}
		if (list.size() == 0) {
			return DSAFactory.getIndexedList();
		}
		WatchRecord[] arr = new WatchRecord[list.size()];
		for (int i = 0; i < list.size(); i++) {
			arr[i] = list.get(i);
		}
		Sorter<WatchRecord> sorter = DSAFactory.getComparisonSorter();
		sorter.sort(arr);
		List<WatchRecord> wlist = DSAFactory.getIndexedList();
		for (int i = arr.length - 1; i >= 0 ; i--) {
			wlist.addLast(arr[i]);
		}
		return wlist;
	}

	/**
	 * Return a list of movie records that contains the top n most frequently
	 * watched movies
	 * 
	 * @param numberOfMovies the n most frequently watched movies to include in the
	 *                       list
	 * @return a list of movie records that contains the top n most frequently
	 *         watched movies
	 */
	public List<Movie> getMostFrequentlyWatchedMovies(int numberOfMovies) {
		

		if (watchlist.size() == 0 || numberOfMovies <= 0) {
			return DSAFactory.getIndexedList();
		}
		
		LinkedListMultimap<String, Integer> wmap = DSAFactory.getMap();
		for (int i = 0; i < watchlist.size(); i++) {
 			WatchRecord r = watchlist.get(i);
 			String id = r.getMovieId();
 			wmap.put(id, 1);
 			
// 			if (wmap.get(id) != null) {
//// 				java.util.List<Integer> val_list = wmap.wmap.get(id)(id);
//// 				System.out.println(wmap.get(id));
//// 				int val = val_list.get(0);
//// 				wmap.put(id, val + 1);
// 			} else {
// 				wmap.put(id, 1);
// 			}
 			
 			
		}
		
		List<MovieEntry> llist = DSAFactory.getIndexedList();
 		for (int i = 0; i < movielist.size(); i++) {
 			Movie movie = movielist.get(i);
 			String id = movie.getId();
 			
 			int val = wmap.get(id).size();
 			if (val != 0) {
 				
 				MovieEntry m = new MovieEntry(movie, val);
 				llist.addLast(m);
 			}
 		}

 		MovieEntry[] entries = new MovieEntry[llist.size()];
 		
 		for (int i = 0; i < llist.size(); i++) {
 			entries[i] = llist.get(i);
 		}
		
 		Sorter<MovieEntry> sorter = DSAFactory.getComparisonSorter();

 		sorter.sort(entries);

 		List<Movie> list = DSAFactory.getIndexedList();
		for (int i = 0; i < entries.length; i++) {
			if (i == numberOfMovies) {
				break;
			}
			list.addLast(entries[i].getMovie());
		}
		return list;
	
	}


	
	/**
	 * Return a list of movie records that have been watched less than a specific
	 * threshold percentage
	 * 
	 * @param threshold the percentages threshold to use, as a whole number
	 * @return a list of movie records that have been watched less than the specified
	 *         threshold percentage
	 */
	public List<Movie> getMoviesByWatchDuration(int threshold) {
		
		if (watchlist.size() == 0) {
			return DSAFactory.getIndexedList();
		}
		LinkedListMultimap<String, Integer> map = DSAFactory.getMap();
		for (int i = 0; i < watchlist.size(); i++) {
			WatchRecord r = watchlist.get(i);
			String id = r.getMovieId();
			int watchtime = r.getWatchTime();
			
			int count = map.get(id).size();
			
			if (count != 0) {
				java.util.List<Integer> val_list = map.get(id);
				int val = val_list.get(0);
				if (val < watchtime) {
					map.removeAll(id);
					//map.remove(id);
					map.put(id, watchtime);
				}
			} else {
				map.put(id, watchtime);
			}
		}
		
		List<MovieEntry> entrylist = DSAFactory.getIndexedList();
		for (int i = 0; i < movielist.size(); i++) {
			Movie movie = movielist.get(i);
			int count = map.get(movie.getId()).size();
			if (count != 0) {
				java.util.List<Integer> val_list = map.get(movie.getId());
				int val = val_list.get(0);
				int percent = val * 100 / movie.getRuntime();
				if (percent < threshold) {
					MovieEntry e = new MovieEntry(movie, percent);
					entrylist.addLast(e);
				}
			}
		}
		MovieEntry[] entries = new MovieEntry[entrylist.size()];
		//int count = 0;
		for (int i = 0; i < entrylist.size(); i++) {
			entries[i] = entrylist.get(i);
		}
		
		Sorter<MovieEntry> sorter = DSAFactory.getComparisonSorter();
		sorter.sort(entries);
		
		List<Movie> list = DSAFactory.getIndexedList();
		for (int i = 0; i < entries.length; i++) {
			list.addLast(entries[i].getMovie());
		}
		return list;
		
	}
	
	/**
	 * Movie Entry
	 * @author Eason Li
	 *
	 */
	private class MovieEntry implements Comparable<MovieEntry>  {
		/**
		 * movie
		 */
		private Movie movie;
		/**
		 * value
		 */
		private Integer value;
		/**
		 * id
		 */
		private String id;
		
//		public MovieEntry(Entry<Movie, Integer> entry) {
//			this.setMovie(entry.getKey());
//			this.setValue(entry.getValue());
//		}
		
		/**
		 * movie entry constructor
		 * @param movie movie 
		 * @param value value
		 */
		public MovieEntry(Movie movie, Integer value) {
			this.setMovie(movie);
			this.setValue(value);
			this.setId(movie.getId());
		}
		
		/**
		 * get value
		 * @return int value
		 */
		public Integer getValue() {
			return value;
		}

		/**
		 * set value
		 * @param value set value
		 */
		public void setValue(Integer value) {
			this.value = value;
		}

		/**
		 * get movie
		 * @return movie
		 */
		public Movie getMovie() {
			return movie;
		}

		/**
		 * set movie
		 * @param movie movie
		 */ 
		public void setMovie(Movie movie) {
			this.movie = movie;
		}
		
		/**
		 * get id
		 * @return id
		 */
		public String getId() {
			return id;
		}

		/**
		 * set id
		 * @param id id
		 */
		public void setId(String id) {
			this.id = id;
		}


		/**
		 * compare to method
		 * @param o movie entry
		 * @return integer to show the comparison
		 */
		@Override
		public int compareTo(MovieEntry o) {
			if (this.getValue() > o.getValue()) {
	            return -1;
	        } else if (this.getValue() < o.getValue()) {
	        	return 1;
	        } else {
	        	if (this.getMovie().getTitle().compareTo(o.getMovie().getTitle()) < 0) {
		        	return -1;
		    	} else if (this.getMovie().getTitle().compareTo(o.getMovie().getTitle()) > 0) {
		    		return 1;
		    	} else {
		    		if (this.getId().compareTo(o.getId()) < 0) {
		    			return -1;
		    		} 
		    		else if (this.getId().compareTo(o.getId()) > 0) {
		    			return 1;
		    		} else {
		    			return 0;
		    		}
		    	}
	        }
			
		}

		
	}
	
	

	
	
}