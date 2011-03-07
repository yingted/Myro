package Myro;

import java.util.Random;

/**
 * Miscellaneous methods for Myro/Java programs
 * 
 * @author Douglas Harms 
 * @version 1.0
 */
public class MyroUtils
{

    private static Random randomSeq;
    
    // static constructor
    static
    {
        randomSeq = new Random();
    }
    
    /**
     * Cause the current thread to sleep for numSeconds.
     * <p><p>
     * <b>Precondition:</b> numSeconds >= 0.0
     * 
     * @param numSeconds The length of time to sleep.
     */
    public static void wait( double numSeconds )
    {
        assert numSeconds >= 0.0 : "numSeconds must be >= 0.0";
        try
        {
            Thread.sleep( (int)(numSeconds * 1000.0) );
        } catch (InterruptedException e) {}

    }
    
    /**
     * Returns a random integer within a specified range.
     * <p><p>
     * <b>Precondition:</b> low <= high
     * 
     * @param low Low end of range
     * @param high High end of range
     * @return A uniformly distributed random int between low (inclusive) and high (inclusive)
     */
    public static int randomInt( int low, int high )
    {
        assert low <= high : "low cannot be greater than high";
        
        return randomSeq.nextInt( high-low+1 ) + low ;
    }

}
