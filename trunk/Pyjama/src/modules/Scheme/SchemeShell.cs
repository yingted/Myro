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
using Gdk;
using GtkSourceView;
using System;
using System.IO;
using System.Collections.Generic;
using System.Threading;

using PyjamaInterfaces;
using System.Text;

//using Scheme;

public class SchemeShell: BaseShell, PyjamaInterfaces.IShell
{
    public SchemeShell() : base()
    {
	mgr = new SourceLanguagesManager();
        language = mgr.GetLanguageFromMimeType("text/plain");
        
        // Set up syntax highlighting
        buffer = new SourceBuffer(language);
        buffer.Highlight = true;
        source_view = new ShellSourceView(buffer, "> ");
        
        // Change the font to fixed-width:
        Pango.FontDescription font = new Pango.FontDescription();
        font.Family = "Monospace";
        source_view.ModifyFont(font);
        
        // Event Handlers:
        source_view.WrapMode = Gtk.WrapMode.Word;
        source_view.AutoIndent = true;
        source_view.Execute += execute;

// Encoding encoding = Encoding.UTF8;
        // Probably a more direct way to do these things:
//         command("import sys");
//         command("sys.path.append('.')");
//         command("sys.path.append('python')");
//         command("sys.path.append('DLLs')");
//         buffer.Text += "# Python " + command("sys.version") + "\n" +
//                        "# " + command("sys.copyright") + "\n";
//         command("del sys");
        
        source_view.AddPrompt();
    }

    // Evaluates an expression or executes a statement.
    // Call this for user command-line input when you don't know if
    // the string is an expression or statement.
    public override string command(string line)
    {
	string retval = "ok";
        return retval;
    }

}

