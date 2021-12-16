package packFlix.EL.ui;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.text.ParseException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;

import chainHash.movie.manager.ChainHashManager;
import hashMultiMap.movie.manager.HashMultiMapManager;
import linearProbingHash.movie.manager.LinearProbingManager;
import linkedHashMultiMap.movie.manager.LinkedHashMultiMapManager;
import redBlackTree.movie.manager.RedBlackTreeManager;
import tree.movie.manager.TreeManager;

public class PackFlixUI {

    public static BufferedReader             cin = new BufferedReader( new InputStreamReader( System.in ) );

    private static ChainHashManager          ch;
    private static HashMultiMapManager       hm;
    private static LinkedHashMultiMapManager lh;
    private static LinearProbingManager      lp;
    private static RedBlackTreeManager       rb;
    private static TreeManager               tm;

    public static void displayMenu () {
        System.out.println( "   PackFlix File Generator" );
        System.out.println( "A - Chain Hash" );
        System.out.println( "B - Hash MultiMap" );
        System.out.println( "C - Linked Hash MultiMap" );
        System.out.println( "D - Linear Probing" );
        System.out.println( "E - Red Black Tree" );
        System.out.println( "F - Tree Map" );
        System.out.print( "\nEnter choice: " );

    }

    public static void main ( String[] args ) throws IOException, ParseException {
        final DateTimeFormatter dtf = DateTimeFormatter.ofPattern( "yyyy/MM/dd HH:mm:ss" );
        final LocalDateTime now = LocalDateTime.now();

        System.out.println();
        System.out.println( ".. initializing PackFlix = rBed-bgmc" );
        System.out.println( "   date = " + dtf.format( now ) );
        System.out.println( "   homeDir = " + System.getProperty( "user.home" ) );
        System.out.println( ".. global path to rBed-bgmc" );
        System.out.println( "   globPath[['PackFlix']] = " + System.getProperty( "user.dir" ) + "\n" );

        String choice = "";

        displayMenu();

        choice = cin.readLine();
        System.out.println();

        System.out.println( "Please enter the range of the file size" );
        System.out.print( "Lower bound (2 to 20): " );
        final int lower = Integer.parseInt( cin.readLine() );
        System.out.print( "Upper bound (" + lower + " to 20): " );
        final int upper = Integer.parseInt( cin.readLine() );

        System.out.println();
        readFile( choice, lower, upper );

    }

    public static void readFile ( String choice, int lower, int upper ) throws IOException, ParseException {

        final ArrayList<File> arr_movie = new ArrayList<File>( 10 );
        final ArrayList<File> arr_watch = new ArrayList<File>( 10 );

        System.out.println( "Please enter the folder path that contains all data file: " );
        final File folder = new File( cin.readLine().trim() );

        for ( final File fileEntry : folder.listFiles() ) {
            if ( fileEntry.isDirectory() ) {
                continue;
            }
            else {
                if ( fileEntry.getName().startsWith( "movie" ) ) {
                    arr_movie.add( fileEntry );
                }
                else if ( fileEntry.getName().startsWith( "watch" ) ) {
                    arr_watch.add( fileEntry );
                }

            }
        }

        retrieve_file( arr_movie, arr_watch, choice, lower, upper );

        System.out.println( "Files loaded successfully!\n" );
    }

    public static void retrieve_file ( ArrayList<File> movielist, ArrayList<File> watchlist, String choice, int lower,
            int upper ) throws ParseException, IOException {

        final ArrayList<Integer> file_size = new ArrayList<Integer>( 10 );
        final ArrayList<Double> arr_read = new ArrayList<Double>( 10 );

        final ArrayList<Double> arr_search = new ArrayList<Double>( 10 );

        for ( int i = lower; i <= upper; i += 2 ) {

            String movie_path = "";
            String watch_path = "";
            for ( int j = 0; j < movielist.size(); j++ ) {
                if ( movielist.get( j ).getAbsolutePath().endsWith( "_" + i + ".csv" ) ) {
                    movie_path = movielist.get( j ).getAbsolutePath();
                    break;
                }
            }
            for ( int j = 0; j < watchlist.size(); j++ ) {
                if ( watchlist.get( j ).getAbsolutePath().endsWith( "_" + i + ".csv" ) ) {
                    watch_path = watchlist.get( j ).getAbsolutePath();
                    break;
                }
            }

            System.out.println( "Processing files in size 2^" + i + "\n" );

            file_size.add( i );
            if ( choice.equalsIgnoreCase( "A" ) ) {
                read_ch( movie_path, watch_path, arr_read, arr_search, i );
            }
            else if ( choice.equalsIgnoreCase( "B" ) ) {
                read_hm( movie_path, watch_path, arr_read, arr_search, i );
            }
            else if ( choice.equalsIgnoreCase( "C" ) ) {
                read_lh( movie_path, watch_path, arr_read, arr_search, i );
            }
            else if ( choice.equalsIgnoreCase( "D" ) ) {
                read_lp( movie_path, watch_path, arr_read, arr_search, i );
            }
            else if ( choice.equalsIgnoreCase( "E" ) ) {
                read_rb( movie_path, watch_path, arr_read, arr_search, i );
            }
            else if ( choice.equalsIgnoreCase( "F" ) ) {
                read_tm( movie_path, watch_path, arr_read, arr_search, i );
            }

            System.out.println();
        }

        System.out.println( "Enter the output file name: " );
        final String output_path = cin.readLine();

        write_to_file( output_path, file_size, arr_read, arr_search, choice );

    }

    public static void write_to_file ( String output_path, ArrayList<Integer> file_size, ArrayList<Double> read,
            ArrayList<Double> search, String choice ) throws IOException {

        final File f = new File( output_path.trim() );
        String name = "";
        if ( choice.equalsIgnoreCase( "A" ) ) {
            name = "ChainHash";
        }
        else if ( choice.equalsIgnoreCase( "B" ) ) {
            name = "HashMMap";
        }
        else if ( choice.equalsIgnoreCase( "C" ) ) {
            name = "LinkedHashMMap";
        }
        else if ( choice.equalsIgnoreCase( "D" ) ) {
            name = "LinearProbing";
        }
        else if ( choice.equalsIgnoreCase( "E" ) ) {
            name = "RedBlack";
        }
        else if ( choice.equalsIgnoreCase( "F" ) ) {
            name = "Tree";
        }
        final FileWriter bw = new FileWriter( f );

        String hostname = "Unknown";

        try {
            InetAddress addr;
            addr = InetAddress.getLocalHost();
            hostname = addr.getHostName();
        }
        catch ( final UnknownHostException ex ) {
            System.out.println( "Hostname can not be resolved" );
        }
        final String arch = System.getProperty( "os.arch" );

        final DateTimeFormatter dtf = DateTimeFormatter.ofPattern( "yyyy/MM/dd HH:mm:ss" );
        final LocalDateTime now = LocalDateTime.now();

        bw.write( "# file               = " + output_path + "\n" + "# userId             = "
                + System.getProperty( "user.name" ) + "\n" + "# cpuName            = " + arch + "\n"
                + "# sysName            = " + hostname + "\n" + "# data_type          = " + name + "\n"
                + "# date               = " + dtf.format( now ) + "\n" + "# homeDir            = "
                + System.getProperty( "user.home" ) + "\n" + "# userDir            = "
                + System.getProperty( "user.dir" ) + "\n" );

        bw.write( "Size(2^x)," + "Java_read_" + name + "," + "Java_search_" + name + "\n" );
        for ( int i = 0; i < file_size.size(); i++ ) {
            bw.write( file_size.get( i ) + "," + read.get( i ) + "," + search.get( i ) + "\n" );
        }
        bw.close();
    }

    private static void read_ch ( String movie_path, String watch_path, ArrayList<Double> read,
            ArrayList<Double> search, int i ) throws ParseException, UnsupportedEncodingException, IOException {

        double startTime = 0;
        double elapsedRead = 0;
        double elapsedSearch = 0;

        startTime = System.currentTimeMillis();
        ch = new ChainHashManager( movie_path, watch_path );
        elapsedRead = System.currentTimeMillis() - startTime;

        startTime = System.currentTimeMillis();
        System.out.println( ch.getTopMoviesReport( 10 ) );
        elapsedSearch = System.currentTimeMillis() - startTime;

        elapsedRead /= 1000;
        elapsedSearch /= 1000;
        read.add( elapsedRead );
        search.add( elapsedSearch );
        System.out.println( "   Processing Chain Hash with 2^" + i + " lines" );
        System.out.println( "...read time    = " + elapsedRead );
        System.out.println( "...search time  = " + elapsedSearch );

    }

    private static void read_hm ( String movie_path, String watch_path, ArrayList<Double> read,
            ArrayList<Double> search, int i ) throws FileNotFoundException, ParseException {

        double startTime = 0;
        double elapsedRead = 0;
        double elapsedSearch = 0;

        startTime = System.currentTimeMillis();
        hm = new HashMultiMapManager( movie_path, watch_path );
        elapsedRead = System.currentTimeMillis() - startTime;

        startTime = System.currentTimeMillis();
        System.out.println( hm.getTopMoviesReport( 10 ) );
        elapsedSearch = System.currentTimeMillis() - startTime;

        elapsedRead /= 1000;
        elapsedSearch /= 1000;
        read.add( elapsedRead );
        search.add( elapsedSearch );
        System.out.println( "   Processing Hash Multimap with 2^" + i + " lines" );
        System.out.println( "...read time    = " + elapsedRead );
        System.out.println( "...search time  = " + elapsedSearch );

    }

    private static void read_lh ( String movie_path, String watch_path, ArrayList<Double> read,
            ArrayList<Double> search, int i ) throws FileNotFoundException, ParseException {

        double startTime = 0;
        double elapsedRead = 0;
        double elapsedSearch = 0;

        startTime = System.currentTimeMillis();
        lh = new LinkedHashMultiMapManager( movie_path, watch_path );
        elapsedRead = System.currentTimeMillis() - startTime;

        startTime = System.currentTimeMillis();
        System.out.println( lh.getTopMoviesReport( 10 ) );
        elapsedSearch = System.currentTimeMillis() - startTime;

        elapsedRead /= 1000;
        elapsedSearch /= 1000;
        read.add( elapsedRead );
        search.add( elapsedSearch );
        System.out.println( "   Processing Linked Hash Multimap with 2^" + i + " lines" );
        System.out.println( "...read time    = " + elapsedRead );
        System.out.println( "...search time  = " + elapsedSearch );

    }

    private static void read_lp ( String movie_path, String watch_path, ArrayList<Double> read,
            ArrayList<Double> search, int i ) throws FileNotFoundException, ParseException {

        double startTime = 0;
        double elapsedRead = 0;
        double elapsedSearch = 0;

        startTime = System.currentTimeMillis();
        lp = new LinearProbingManager( movie_path, watch_path );
        elapsedRead = System.currentTimeMillis() - startTime;

        startTime = System.currentTimeMillis();
        System.out.println( lp.getTopMoviesReport( 10 ) );
        elapsedSearch = System.currentTimeMillis() - startTime;

        elapsedRead /= 1000;
        elapsedSearch /= 1000;
        read.add( elapsedRead );
        search.add( elapsedSearch );
        System.out.println( "   Processing Linear Probing with 2^" + i + " lines" );

        System.out.println( "...read time    = " + elapsedRead );
        System.out.println( "...search time  = " + elapsedSearch );

    }

    private static void read_rb ( String movie_path, String watch_path, ArrayList<Double> read,
            ArrayList<Double> search, int i ) throws FileNotFoundException, ParseException {

        double startTime = 0;
        double elapsedRead = 0;
        double elapsedSearch = 0;

        startTime = System.currentTimeMillis();
        rb = new RedBlackTreeManager( movie_path, watch_path );
        elapsedRead = System.currentTimeMillis() - startTime;

        startTime = System.currentTimeMillis();
        System.out.println( rb.getTopMoviesReport( 10 ) );
        elapsedSearch = System.currentTimeMillis() - startTime;

        elapsedRead /= 1000;
        elapsedSearch /= 1000;
        read.add( elapsedRead );
        search.add( elapsedSearch );
        System.out.println( "   Processing Red Black Tree with 2^" + i + " lines" );

        System.out.println( "...read time    = " + elapsedRead );
        System.out.println( "...search time  = " + elapsedSearch );

    }

    private static void read_tm ( String movie_path, String watch_path, ArrayList<Double> read,
            ArrayList<Double> search, int i ) throws FileNotFoundException, ParseException {

        double startTime = 0;
        double elapsedRead = 0;
        double elapsedSearch = 0;

        startTime = System.currentTimeMillis();
        tm = new TreeManager( movie_path, watch_path );
        elapsedRead = System.currentTimeMillis() - startTime;

        startTime = System.currentTimeMillis();
        System.out.println( tm.getTopMoviesReport( 10 ) );
        elapsedSearch = System.currentTimeMillis() - startTime;

        elapsedRead /= 1000;
        elapsedSearch /= 1000;
        read.add( elapsedRead );
        search.add( elapsedSearch );
        System.out.println( "   Processing Tree Map with 2^" + i + " lines" );

        System.out.println( "...read time    = " + elapsedRead );
        System.out.println( "...search time  = " + elapsedSearch );

    }

}
