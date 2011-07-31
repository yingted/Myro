/*
 * Myro/Java license - GPL
 * 
 * Myro/Java is a Java implementation of the Myro API, defined by the Institute for Robots in
 * Education (IPRE).  See http://wiki.roboteducation.org for more information.
 * 
 * Copyright 2010-2011 Douglas Harms dharms@depauw.edu
 * 
 * This file is part of Myro/Java.
 * 
 * Myro/Java is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 * 
 * Myro/Java is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Myro/Java.  If not, see <http://www.gnu.org/licenses/>.
 */

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
        imageType = Scribbler.IMAGE_GRAY;
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
            imageType = Scribbler.IMAGE_GRAY;
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

    public MyroGrayImage( byte[] jpegBuf )
    {
        BufferedImage colorImage;
        try {
            ByteArrayInputStream stream = new ByteArrayInputStream( jpegBuf );
            colorImage = ImageIO.read( ImageIO.createImageInputStream( stream ) );
            width  = colorImage.getWidth(null);
            height = colorImage.getHeight(null);
            imageType = Scribbler.IMAGE_GRAY;

        } catch (IOException e) {
            throw new RuntimeException("Could not open passed buffer");
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

    public Color getColor(int x, int y) {
        Color color = new Color(image.getRGB(x, y));
        return toGray(color);
    }

    public int getGray(int x, int y)
    {
        Color color = new Color( image.getRGB(x,y));
        return color.getRed();
    }

    public void setColor(int x, int y, Color c)
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
