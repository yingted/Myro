package Myro;

/**
 * Write a description of class MyroBlobImageInfo here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class MyroBlobImageInfo
{
    private int pixelCount;
    private int averageX;
    private int averageY;

    public MyroBlobImageInfo( int count, int avgX, int avgY )
    {
        pixelCount = count;
        averageX = avgX;
        averageY = avgY;
    }

    public int getPixelCount()
    {
        return pixelCount;
    }

    public int getAverageX()
    {
        return averageX;
    }

    public int getAverageY()
    {
        return averageY;
    }

}
