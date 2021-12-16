package linearProbingHash.movie.manager;

import java.io.FileNotFoundException;
import java.text.ParseException;

import linearProbingHash.dsa.list.List;
import linearProbingHash.movie.data.Movie;
import linearProbingHash.movie.data.WatchRecord;

/**
 * Report manager
 * @author Eason Li
 *
 */
public class LinearProbingManager {

	/**
	 * manager
	 */
	private MovieManager manager;
	/**
	 * indent
	 */
	private static final String INDENT = "   ";
	

	/**
	 * Creates a new ReportManager for generating reports for the MovieManager software
	 * 
	 * @param pathToMovieFile the path to the file that contains movie records
	 * @param pathToWatchFile the path to the file that contains watch records
	 * @throws FileNotFoundException if either input file cannot be found/read/opened
	 * @throws ParseException if the watch record file contains incorrectly formatted date information
	 */
	public LinearProbingManager(String pathToMovieFile, String pathToWatchFile) throws FileNotFoundException, ParseException {
		manager = new MovieManager(pathToMovieFile, pathToWatchFile);
	}

	/**
	 * Returns a report of the most frequently watched movies that contains the top
	 * n movies most watched
	 * 
	 * @param numberOfMovies the number of movies to include in the report
	 * @return a report of the most frequently watched movies
	 */
	public String getTopMoviesReport(int numberOfMovies) {
		
		if (numberOfMovies <= 0) {
			return "Please enter a number > 0.";
		}
		List<Movie> movielist = manager.getMostFrequentlyWatchedMovies(numberOfMovies);
		if (movielist.isEmpty()) {
			return "No movies have been streamed.";
		}
		
		StringBuilder sb = new StringBuilder();
		sb.append("The ");
		sb.append(numberOfMovies);
		sb.append(" most frequently watched movies [\n");
		for (int i = 0; i < movielist.size(); i++) {
			Movie movie = movielist.get(i);
			sb.append(INDENT);
			sb.append(movie.getTitle());
			sb.append(" (");
			sb.append(movie.getYear());
			sb.append(")\n");
		}
		
		sb.append("]");
		return sb.toString();
	}

	/**
	 * Returns a report of movies below a specific watch percentage threshold.
	 * 
	 * @param threshold the percentage threshold (as a whole number)
	 * @return a report of movies below a specific watch percentage threshold
	 */
	public String getMovieCompletionReport(int threshold) {
		
		if (threshold <= 0 || threshold > 100) {
			return "Please enter a percentage completion between 1 and 100.";
		}
		List<Movie> movielist = manager.getMoviesByWatchDuration(threshold);
		if (movielist.isEmpty()) {
			return "No movies are less than " + threshold + "% completed.";
		}
		StringBuilder sb = new StringBuilder();
		sb.append("The movies that have been watched less than ");
		sb.append(threshold);
		sb.append("% [\n");
		for (int i = 0; i < movielist.size(); i++) {
			Movie movie = movielist.get(i);
			sb.append(INDENT);
			sb.append(movie.getTitle());
			sb.append(" (");
			sb.append(movie.getYear());
			sb.append(")\n");
		}
		
		sb.append("]");
		return sb.toString(); 
	}

	/**
	 * Return a report of dates on which a specific movie was watched
	 * 
	 * @param title the title of the movie for which to retrieve watch dates
	 * @return a report of dates on which a specific movie was watched
	 */
	public String getWatchDates(String title) {
		
		if (title.equals("")) {
			return "Please enter a valid movie title";
		}
		List<WatchRecord> watchlist = manager.getWatchHistory(title);
		if (watchlist.isEmpty()) {
			return "No watch history for \"" + title + "\".";
		}
		StringBuilder sb = new StringBuilder();
		sb.append("The movie \"");
		sb.append(title);
		sb.append("\" was streamed on [\n");
		for (int i = 0; i < watchlist.size(); i++) {
			WatchRecord record = watchlist.get(i);
			String month = "";
			if (record.getDate().getMonthValue() < 10) {
				month = "0" + record.getDate().getMonthValue();
			} else {
				month = "" + record.getDate().getMonthValue();
			}
			String day = "";
			if (record.getDate().getDayOfMonth() < 10) {
				day = "0" + record.getDate().getDayOfMonth();
			} else {
				day = "" + record.getDate().getDayOfMonth();
			}
			sb.append(INDENT);
			sb.append(month);
			sb.append("/");
			sb.append(day);
			sb.append("/");
			sb.append(record.getDate().getYear());
			sb.append("\n");
		
		}
		
		sb.append("]");
		return sb.toString();
	}
}