package Myro;

 import java.io.*;
 import java.awt.*;
 import java.awt.image.*;
 import java.net.*;
 import javax.imageio.*;

public class MyroGrayImage extends MyroImage  {

    // create a blank w-by-h image
    public MyroGrayImage(int w, int h) {
        image = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_GRAY);
        width = w;
        height = h;
        imageType = MyroImage.MYRO_IMAGE_GRAY;
    }

    // create an image by reading in the PNG, GIF, or JPEG from a filename
    public MyroGrayImage(String filename) {
        BufferedImage colorImage;

        try {
            // try to read from file in working directory
            File file = new File(filename);
            if (file.isFile()) {
                colorImage = ImageIO.read(file);
            }

            // now try to read from file in same directory as this .class file
            else {
                URL url = getClass().getResource(filename);
                if (url == null) { url = new URL(filename); }
                colorImage = ImageIO.read(url);
            }
            
            width = colorImage.getWidth( null );
            height = colorImage.getHeight( null );
            imageType = MyroImage.MYRO_IMAGE_GRAY;
        }
        catch (IOException e) {
            // e.printStackTrace();
            throw new RuntimeException("Could not open file: " + filename);
        }

        // check that image was read in
        if (colorImage == null) {
            throw new RuntimeException("Invalid image file: " + filename);
        }

        // convert to grayscale
        int w = colorImage.getWidth(null);
        int h = colorImage.getHeight(null);
        image = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_GRAY);
        for (int i = 0; i < w; i++) {
            for (int j = 0; j < h; j++) {
                Color color = new Color(colorImage.getRGB(i, j));
                Color gray = toGray(color);
                image.setRGB(i, j, gray.getRGB());
            }
        }
    }

    public Color get(int x, int y) {
        Color color = new Color(image.getRGB(x, y));
        return toGray(color);
    }
    
    public int getGray(int x, int y)
    {
        Color color = new Color( image.getRGB(x,y));
        return color.getRed();
    }

    public void set(int x, int y, Color c)
    {
        assert c!=null : "Color must be non-null";
        
        Color gray = toGray(c);
        image.setRGB(x, y, gray.getRGB());
    }
    
    public void setGray(int x, int y, int grayLevel)
    {
        Color gray = new Color( grayLevel, grayLevel, grayLevel );
        image.setRGB(x, y, gray.getRGB() );
    }

}
