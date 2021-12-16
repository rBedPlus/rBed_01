package linearProbingHash.movie.data;

import java.time.LocalDate;


public class WatchRecord implements Comparable<WatchRecord> {
	private String movieId;
	private LocalDate date;
	private int watchTime;
	
	public WatchRecord(String movieId, LocalDate date, int watchTime) {
		setMovieId(movieId);
		setDate(date);
		setWatchTime(watchTime);
	}

	/**
	 * @return the movieId
	 */
	public String getMovieId() {
		return movieId;
	}

	/**
	 * @param movieId the movieId to set
	 */
	public void setMovieId(String movieId) {
		this.movieId = movieId;
	}

	/**
	 * @return the date
	 */
	public LocalDate getDate() {
		return date;
	}

	/**
	 * @param date the date to set
	 */
	public void setDate(LocalDate date) {
		this.date = date;
	}

	/**
	 * @return the watchTime
	 */
	public int getWatchTime() {
		return watchTime;
	}

	/**
	 * @param watchTime the watchTime to set
	 */
	public void setWatchTime(int watchTime) {
		this.watchTime = watchTime;
	}
	
	public int compareTo(WatchRecord other) {
	    if (!this.date.equals(other.getDate())) {
	    	return this.date.compareTo(other.getDate()); 
	    }
	      
	    return this.watchTime - other.watchTime;
	}
	
}
