package Myro;

/**
 * Write a description of class MyroBlob here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */

import java.awt.*;

public class MyroBlobSpec
{
    public int y_low, y_high;
    public int u_low, u_high;
    public int v_low, v_high;
    public int threshold;

    public MyroBlobSpec( int yl, int yh, int ul, int uh, int vl, int vh, int t )
    {
        y_low = yl;
        y_high = yh;
        u_low = ul;
        u_high = uh;
        v_low = vl;
        v_high = vh;
        threshold = t;
    }
    
    public MyroBlobSpec ()
    {
        y_low = 0;
        y_high = 254;
        u_low = 51;
        u_high = 136;
        v_low = 190;
        v_high = 254;
        threshold = 4;
    }

}
