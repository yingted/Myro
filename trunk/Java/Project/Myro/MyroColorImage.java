package Myro;


 import java.io.*;
 import java.awt.*;
 import java.awt.image.*;
 import java.net.*;
 import javax.imageio.*;
 
public class MyroColorImage extends MyroImage {

    public MyroColorImage(int w, int h) {
        width = w;
        height = h;
        image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
        imageType = MyroImage.MYRO_IMAGE_COLOR;
    }

    public MyroColorImage(String filename) {
        try {
            // try to read from file in working directory
            File file = new File(filename);
            if (file.isFile()) {
                image = ImageIO.read(file);
            }

            // now try to read from file in same directory as this .class file
            else {
                URL url = getClass().getResource(filename);
                if (url == null) { url = new URL(filename); }
                image = ImageIO.read(url);
            }
            width  = image.getWidth(null);
            height = image.getHeight(null);
            imageType = MyroImage.MYRO_IMAGE_COLOR;
        }
        catch (IOException e) {
            throw new RuntimeException("Could not open file: " + filename);
        }

        // check that image was read in
        if (image == null) {
            throw new RuntimeException("Invalid image file: " + filename);
        }
    }

    public Color get(int x, int y)
    {
        return new Color(image.getRGB(x, y));
    }

    public int getGray(int x, int y)
    {
        return (int) lum( get( x, y ) );
    }
    
    public void set(int x, int y, Color c)
    {
        assert c!=null : "Color must be non-null";

        image.setRGB(x, y, c.getRGB());
    }

    public void setGray( int x, int y, int g)
    {
        assert 0<=g && g<=255 : "Grayscale value out of 0..255 range";
        
        set( x, y, new Color( g, g, g ) );
    }
}
