package bgm_EL.main;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class InputFileReader {

    public InputFileReader () {
        // Empty
    }

    public static boolean[][] readFile ( String filePath ) {

        boolean[][] mat = new boolean[][] {};
        try ( Scanner scan = new Scanner( new FileInputStream( filePath ), "UTF8" ) ) {

            int i = 0;
            while ( scan.hasNextLine() ) {
                final String line = scan.nextLine();

                if ( line.trim().equals( "" ) ) {
                    break;
                }
                if ( line.startsWith( "p" ) ) {
                    mat = getMatrix( line );
                    if ( filePath.endsWith( "cnfW" ) ) {
                        scan.nextLine();
                    }
                    System.out.println( line );
                }
                else if ( !line.startsWith( "c" ) ) {
                    final int cols = mat[0].length;

                    final boolean[] rowData = processData( line, cols );
                    // System.out.println( rowData[cols - 1] );
                    for ( int j = 0; j < rowData.length; j++ ) {
                        mat[i][j] = rowData[j];
                    }
                    i++;
                }

            }

        }
        catch ( final FileNotFoundException e ) {
            throw new IllegalArgumentException( "File not found: " + e.getMessage() );
        }

        // for (int i = 0; i < mat[0].length; i++) {
        // for (int j = 0; j < mat.length; j++) {
        // System.out.print(mat[j][i] + " ");
        // }
        // System.out.println();
        // }
        //
        return mat;
    }

    private static boolean[] processData ( String line, int cols ) {

        line = line.trim();
        final Scanner scan = new Scanner( line );
        scan.useDelimiter( "\\s+" );
        final boolean[] arr = new boolean[cols];
        for ( int i = 0; i < cols; i++ ) {
            arr[i] = false;
        }

        while ( scan.hasNextInt() ) {

            final int val = scan.nextInt();
            if ( val == 0 ) {
                break;
            }
            arr[val - 1] = true;

        }

        scan.close();

        return arr;
    }

    public static boolean[][] getMatrix ( String line ) {

        final Scanner scan = new Scanner( line );
        scan.useDelimiter( " " );

        while ( !scan.hasNextInt() ) {
            scan.next();
        }
        // scan.next();
        final int col = scan.nextInt();
        final int row = scan.nextInt();
        scan.close();

        // System.out.println( col );

        final boolean[][] mat = new boolean[row][col];
        for ( int i = 0; i < mat[0].length; i++ ) {
            for ( int j = 0; j < mat.length; j++ ) {
                mat[j][i] = false;
            }
        }
        return mat;

    }

}
