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

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Iterator to iterate over all pixels in a MyroImage.  Note: remove() has no effect.
 * 
 * @author Douglas Harms
 * @version 1.0 - 31 July 2011
 */
public class MyroPixelIterator implements Iterator<MyroPixel>
{
    public MyroPixelIterator( MyroImage image )
    {
        _image = image;
        _x = 0;
        _y = 0;
        _width = _image.getWidth();
        _height = _image.getHeight();
    }

    public MyroPixel next() throws NoSuchElementException
    {
        if( _y < _height && _x < _width )
        {
            // (_x, _y) is a valid coordinate.  Get the pixel that's there
            MyroPixel retVal = _image.getPixel( _x, _y );

            // now increment the position
            _x++;
            if( _x >= _width )
            {
                _x = 0;
                _y++;
            }

            // return the pixel
            return retVal;
        }
        else
        // not a valid position so throw an exception
            throw new NoSuchElementException();
    }

    public boolean hasNext()
    {
        return _x < _width && _y < _height;
    }

    public void remove()
    {
    }
    
    private int _x, _y;
    private int _width, _height;
    private MyroImage _image;
}
