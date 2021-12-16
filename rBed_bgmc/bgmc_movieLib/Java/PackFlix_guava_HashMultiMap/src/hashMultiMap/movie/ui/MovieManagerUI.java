package hashMultiMap.movie.ui;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.ParseException;
import java.util.Random;

import hashMultiMap.movie.manager.HashMultiMapManager;

/**
 * Movie manager UI
 * @author Eason Li
 *
 */
public class MovieManagerUI {

	/**
	 * manager
	 */
	private static HashMultiMapManager manager;
	
	/**
	 * constructor
	 * @param pathToMovieFile movie file
	 * @param pathToWatchFile watch file
	 * @throws FileNotFoundException file not found exception
	 * @throws ParseException parse exception
	 */
//	public MovieManagerUI(String pathToMovieFile, String pathToWatchFile) throws FileNotFoundException, ParseException {
//		manager = new ReportManager(pathToMovieFile, pathToWatchFile);
//	}
//	
	public static BufferedReader cin = new BufferedReader(new InputStreamReader(System.in));
	
	
	/**
	 * main method
	 * @param args args
	 * @throws IOException io exception
	 * @throws ParseException 
	 */
public static void main(String[] args) throws IOException {
		
		String choice = "";
		
		System.out.println("Please enter the seedInit: ");
		int seedInit = Integer.parseInt(cin.readLine());
		System.out.println();
		
		try {
			readFile(seedInit);
			do {
				displayMenu();
				choice = cin.readLine();
				if (choice.equalsIgnoreCase("A")) {
					readFile(seedInit);
				} else if (choice.equalsIgnoreCase("B")) {
					topMovieReport(seedInit);
				} else if (choice.equalsIgnoreCase("C")) {
					completionReport(seedInit);
				} else if (choice.equalsIgnoreCase("D")) {
					watchDateReport(seedInit);
				}
			} while(!choice.equalsIgnoreCase("Q"));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			System.out.println(e.getMessage());
		}	
	}
	
	public static void displayMenu() {
		System.out.println("   JAVA MOVIE MANAGER");
		System.out.println("A - CHANGE INPUT RECORD FILES");
		System.out.println("B - GET TOP MOVIE REPORT");
		System.out.println("C - GET MOVIE COMPLETION REPORT");
		System.out.println("D - GET MOVIE HISTORY REPORT");
		System.out.println("Q - Quit\n");
		System.out.print("Enter choice: ");
	}
	
	public static void readFile(int seedInit) throws IOException {
		
		Random r = new Random();
		r.setSeed(seedInit);
		
		boolean fileWorked = true;
		do {
			fileWorked = true;
			System.out.print("Enter Movie Record File: ");
			String movie_path = cin.readLine().trim();
			System.out.print("Enter History Record File: ");
			String history_path = cin.readLine().trim();
			System.out.println();
			try {
				long startTime = System.currentTimeMillis(); 
				manager = new HashMultiMapManager(movie_path, history_path);
				long endTime = System.currentTimeMillis();
				long elapsedTime = endTime - startTime;
				System.out.println("Elapsed Time (ms): " + elapsedTime);
			} catch(Exception e) {
				fileWorked = false;
				System.out.println("Input data files are not found!\n");
			}
		} while (!fileWorked);
		System.out.println("Files loaded successfully!");
	}
	
	public static void topMovieReport(int seedInit) throws NumberFormatException, IOException {
		
		Random r = new Random();
		r.setSeed(seedInit);
		
		System.out.print("Enter the number of movies: ");
		int numberOfMovies = Integer.parseInt(cin.readLine());
		System.out.println();
		long startTime = System.currentTimeMillis(); 
		System.out.println(manager.getTopMoviesReport(numberOfMovies));
		long endTime = System.currentTimeMillis();
		long elapsedTime = endTime - startTime;
		System.out.println("Elapsed Time (ms): " + elapsedTime);
	}
	
	public static void completionReport(int seedInit) throws NumberFormatException, IOException {
		
		Random r = new Random();
		r.setSeed(seedInit);
		
		System.out.print("Enter the threshold that movies been watched: ");
		int threshold = Integer.parseInt(cin.readLine());
		System.out.println();
		long startTime = System.currentTimeMillis(); 
		System.out.println(manager.getMovieCompletionReport(threshold));
		System.out.println();
		long endTime = System.currentTimeMillis();
		long elapsedTime = endTime - startTime;
		System.out.println("Elapsed Time (ms): " + elapsedTime);
	}
	
	public static void watchDateReport(int seedInit) throws IOException {
		
		Random r = new Random();
		r.setSeed(seedInit);
		
		System.out.print("Enter the title of the movie: ");
		String title = cin.readLine().trim();
		System.out.println();
		long startTime = System.currentTimeMillis(); 
		System.out.println(manager.getWatchDates(title));
		System.out.println();
		long endTime = System.currentTimeMillis();
		long elapsedTime = endTime - startTime;
		System.out.println("Elapsed Time (ms): " + elapsedTime);
	}
	
}