/*********************************************************************
 *
 * Copyright (c) 2008 Douglas S. Blank
 *
 * This source code is subject to terms and conditions of the
 * Microsoft Public License. A copy of the license can be found in the
 * License.html file at the root of this distribution. If you cannot
 * locate the Microsoft Public License, please send an email to
 * dlr@microsoft.com. By using this source code in any fashion, you
 * are agreeing to be bound by the terms of the Microsoft Public
 * License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *********************************************************************/

using Gtk;

using PyjamaInterfaces;

public class Pyjama
{
    public static void Main(string[] args)
    {
        Application.Init();
        PyjamaGUI gui = new PyjamaGUI(args);
        gui.ShowAll();
        Application.Run();
    }
}