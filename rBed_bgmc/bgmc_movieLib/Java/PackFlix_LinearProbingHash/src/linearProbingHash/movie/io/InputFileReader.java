package linearProbingHash.movie.io;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;

import linearProbingHash.dsa.list.*;
import linearProbingHash.movie.data.*;
import linearProbingHash.movie.factory.DSAFactory;

public class InputFileReader {

	public InputFileReader() {
		// Empty
	}
	
	public static List<Movie> readMovieFile(String filePath) {
		
		List<Movie> list = DSAFactory.getIndexedList();
		try(Scanner scan = new Scanner(new FileInputStream(filePath), "UTF8"))
		{
			scan.nextLine(); // SKIP HEADER LINE
			while (scan.hasNextLine()) {
				String line = scan.nextLine();
				Movie movie = processMovie(line);
				list.addLast(movie);
			}
			
		} catch (FileNotFoundException e) {
			throw new IllegalArgumentException("File not found: " + e.getMessage());
		}
		
		return list;
	}
	
	private static Movie processMovie(String line) {
		try {
			Scanner scan = new Scanner(line);
			scan.useDelimiter(",");
			String id = scan.next().trim();
			String title = scan.next().trim();
			int year = scan.nextInt();
			int runtime = scan.nextInt();
			String genres = "";
		    while (scan.hasNext()) {
		      genres = String.valueOf(genres) + scan.next().trim();
		      if (scan.hasNext())
		        genres = String.valueOf(genres) + ","; 
		    } 
			scan.close();
			
			return new Movie(id, title, year, genres.split(","), runtime);
		} catch (Exception e) {
			throw new IllegalArgumentException("Line formatted incorrect!" + e.getMessage());
		}
	}
	
	public static List<WatchRecord> readHistoryFile(String filePath) {
		List<WatchRecord> list = new ArrayBasedList<WatchRecord>();
		try(Scanner scan = new Scanner(new FileInputStream(filePath), "UTF8"))
		{
			scan.nextLine(); // SKIP HEADER LINE
			
			while (scan.hasNextLine()) {
				String line = scan.nextLine();
				WatchRecord record = processHistory(line);
				list.addLast(record);
			}
			
		} catch (FileNotFoundException e) {
			throw new IllegalArgumentException("File not found: " + e.getMessage());
		}
		
		
		return list;
	}
	
	private static WatchRecord processHistory(String line) {
		try {
			Scanner scan = new Scanner(line);
			scan.useDelimiter(",");
			String id = scan.next().trim();
			String d = scan.next().trim();
			DateTimeFormatter sdf = DateTimeFormatter.ofPattern("MM/dd/yyyy");
		    LocalDate date = LocalDate.parse(d, sdf);
			int watchTime = scan.nextInt();
			scan.close();
			
			return new WatchRecord(id, date, watchTime);
		} catch (Exception e) {
			throw new IllegalArgumentException("Line formatted incorrect" + e.getMessage());
		}
		
	}
}
