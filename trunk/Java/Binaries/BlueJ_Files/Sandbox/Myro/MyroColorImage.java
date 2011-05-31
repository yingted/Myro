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

public class MyroColorImage extends MyroImage {

    public MyroColorImage(int w, int h) {
        width = w;
        height = h;
        image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
        imageType = Scribbler.IMAGE_COLOR;
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
            imageType = Scribbler.IMAGE_COLOR;
        }
        catch (IOException e) {
            throw new RuntimeException("Could not open file: " + filename);
        }

        // check that image was read in
        if (image == null) {
            throw new RuntimeException("Invalid image file: " + filename);
        }
    }

    public MyroColorImage( byte[] jpegBuf )
    {
        try {
            ByteArrayInputStream stream = new ByteArrayInputStream( jpegBuf );
            image = ImageIO.read( ImageIO.createImageInputStream( stream ) );
            width  = image.getWidth(null);
            height = image.getHeight(null);
            imageType = Scribbler.IMAGE_COLOR;

        } catch (IOException e) {
            throw new RuntimeException("Could not open passed buffer");
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
