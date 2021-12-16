package chainHash.movie.io;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;

import chainHash.dsa.list.ArrayBasedList;
import chainHash.dsa.list.List;
import chainHash.movie.data.Movie;
import chainHash.movie.data.WatchRecord;
import chainHash.movie.factory.DSAFactory;

public class InputFileReader {

    public InputFileReader () {
        // Empty
    }

    public static List<Movie> readMovieFile ( String filePath ) throws UnsupportedEncodingException, IOException {

        final List<Movie> list = DSAFactory.getIndexedList();
        try (
                BufferedReader scan = new BufferedReader(
                        new InputStreamReader( new FileInputStream( filePath ), "UTF8" ) ) ) {
            String line;

            scan.readLine();
            while ( ( line = scan.readLine() ) != null ) {
                final Movie movie = processMovie( line );
                list.addLast( movie );
            }

        }
        catch ( final FileNotFoundException e ) {
            throw new IllegalArgumentException( "File not found: " + e.getMessage() );
        }

        return list;
    }

    private static Movie processMovie ( String line ) {
        try {
            final Scanner scan = new Scanner( line );
            scan.useDelimiter( "," );
            final String id = scan.next().trim();
            final String title = scan.next().trim();
            final int year = scan.nextInt();
            final int runtime = scan.nextInt();
            String genres = "";
            while ( scan.hasNext() ) {
                genres = String.valueOf( genres ) + scan.next().trim();
                if ( scan.hasNext() ) {
                    genres = String.valueOf( genres ) + ",";
                }
            }
            scan.close();

            return new Movie( id, title, year, genres.split( "," ), runtime );
        }
        catch ( final Exception e ) {
            throw new IllegalArgumentException( "Line formatted incorrect!" + e.getMessage() );
        }
    }

    public static List<WatchRecord> readHistoryFile ( String filePath )
            throws UnsupportedEncodingException, IOException {
        final List<WatchRecord> list = new ArrayBasedList<WatchRecord>();
        try (

                BufferedReader scan = new BufferedReader(
                        new InputStreamReader( new FileInputStream( filePath ), "UTF8" ) ) ) {

            String line;
            scan.readLine();
            while ( ( line = scan.readLine() ) != null ) {
                final WatchRecord movie = processHistory( line );
                list.addLast( movie );
            }

        }
        catch ( final FileNotFoundException e ) {
            throw new IllegalArgumentException( "File not found: " + e.getMessage() );
        }

        return list;
    }

    private static WatchRecord processHistory ( String line ) {
        try {
            final Scanner scan = new Scanner( line );
            scan.useDelimiter( "," );
            final String id = scan.next().trim();
            final String d = scan.next().trim();
            final DateTimeFormatter sdf = DateTimeFormatter.ofPattern( "MM/dd/yyyy" );
            final LocalDate date = LocalDate.parse( d, sdf );
            final int watchTime = scan.nextInt();
            scan.close();

            return new WatchRecord( id, date, watchTime );
        }
        catch ( final Exception e ) {
            throw new IllegalArgumentException( "Line formatted incorrect" + e.getMessage() );
        }

    }
}
