package bgm_EL.ui;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import bgm_EL.main.GFG;
import bgm_EL.main.InputFileReader;

public class bgmUI {

    public static BufferedReader cin = new BufferedReader( new InputStreamReader( System.in ) );

    public static void main ( String[] args ) throws java.lang.Exception {

        final DateTimeFormatter dtf = DateTimeFormatter.ofPattern( "yyyy/MM/dd HH:mm:ss" );
        final LocalDateTime now = LocalDateTime.now();

        System.out.println();
        System.out.println( ".. initializing PackFlix = rBed-bgmc" );
        System.out.println( "   date = " + dtf.format( now ) );
        System.out.println( "   homeDir = " + System.getProperty( "user.home" ) );
        System.out.println( ".. global path to rBed-bgmc" );
        System.out.println( "   globPath[['PackFlix']] = " + System.getProperty( "user.dir" ) + "\n" );

        System.out.println( "Please enter the folder path that contains all data file: " );
        final File folder = new File( cin.readLine().trim() );

        System.out.println( "Enter the output file name: " );
        final String output_path = cin.readLine();

        String hostname = "Unknown";

        try {
            InetAddress addr;
            addr = InetAddress.getLocalHost();
            hostname = addr.getHostName();
        }
        catch ( final UnknownHostException ex ) {
            System.out.println( "Hostname can not be resolved" );
        }

        final File f = new File( output_path.trim() );
        final String arch = System.getProperty( "os.arch" );
        final FileWriter bw = new FileWriter( f );
        bw.write( "# file               = " + output_path + "\n" + "# userId             = "
                + System.getProperty( "user.name" ) + "\n" + "# cpuName            = " + arch + "\n"
                + "# sysName            = " + hostname + "\n" + "# date               = " + dtf.format( now ) + "\n"
                + "# homeDir            = " + System.getProperty( "user.home" ) + "\n" + "# userDir            = "
                + System.getProperty( "user.dir" ) + "\n" );

        bw.write( "instance," + "runtime_read" + "," + "runtime" + "," + "max_matching\n" );

        for ( final File fileEntry : folder.listFiles() ) {
            if ( getFileExtension( fileEntry ).equals( ".cnfU" ) || getFileExtension( fileEntry ).equals( ".cnfW" ) ) {

                final String instanceDef = fileEntry.toString();

                System.out.println( ".. initializing runtime with 'System.currentTimeMillis' before reading" );
                System.out.println( "   file = " + instanceDef );
                final long startTime_r = System.currentTimeMillis();
                final boolean[][] mat = InputFileReader.readFile( instanceDef );

                final long endTime_r = System.currentTimeMillis();
                final long elapsedTime_r = endTime_r - startTime_r;
                System.out.println( ".. reading file completed: " + elapsedTime_r + "ms elapsed" );

                System.out.println(
                        ".. initializing runtime with 'System.currentTimeMillis' before invoking max_bipartite_match" );
                final long startTime = System.currentTimeMillis();
                final GFG m = new GFG( mat.length, mat[0].length );

                final int max_match = m.maxBPM( mat );
                final long endTime = System.currentTimeMillis();
                final long elapsedTime = endTime - startTime;
                System.out.println( ".. computation completed: " + elapsedTime + "ms elapsed" );

                System.out.println();
                System.out.println( " thisFunction       = bgm_max_bipartite_match" );
                System.out.println( " userId         = " + System.getProperty( "user.name" ) );
                System.out.println( " sysName        = " + System.getProperty( "os.name" ) );
                System.out.println( " dateStamp      = " + dtf.format( now ) );
                System.out.println( " instanceDef        = " + instanceDef );
                System.out.println( " max_matching_size  = " + max_match );
                System.out.println( " runtimeReadGraph   = " + elapsedTime_r + "ms" );
                System.out.println( " runtimeGetMatch    = " + elapsedTime + "ms" );

                bw.write( basename( instanceDef ) + "," + elapsedTime_r + "," + elapsedTime + "," + max_match + "\n" );
                System.out.println( "..finishing here" );
            }
        }

        bw.close();
    }

    private static String getFileExtension ( File file ) {
        String extension = "";

        try {
            if ( file != null && file.exists() ) {
                final String name = file.getName();
                extension = name.substring( name.lastIndexOf( "." ) );
            }
        }
        catch ( final Exception e ) {
            extension = "";
        }

        return extension;

    }

    private static String basename ( String path ) {
        String filename = path.substring( path.lastIndexOf( '/' ) + 1 );

        if ( filename == null || filename.equalsIgnoreCase( "" ) ) {
            filename = "";
        }
        return filename;
    }
}
