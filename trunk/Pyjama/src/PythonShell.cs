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
using IronPython.Hosting;
using IronPython.Runtime.Types;

using System.Text;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;

public class PythonShell: BaseShell, PyjamaInterfaces.IShell
{
    private readonly ScriptEngine engine;
    private readonly ScriptRuntime runtime;
    private ScriptScope scope;
    private TextBufferOutputStream output_stream;
    
    public PythonShell() : base()
    {
	mgr = new SourceLanguagesManager();
        language = mgr.GetLanguageFromMimeType("text/x-python");
        
        // Set up syntax highlighting
        buffer = new SourceBuffer(language);
        buffer.Highlight = true;
        source_view = new ShellSourceView(buffer, ">>> ");
        
        // Change the font to fixed-width:
        Pango.FontDescription font = new Pango.FontDescription();
        font.Family = "Monospace";
        source_view.ModifyFont(font);
        
        // Event Handlers:
        source_view.WrapMode = Gtk.WrapMode.Word;
        source_view.AutoIndent = true;
        source_view.Execute += execute;

        output_stream = new TextBufferOutputStream(buffer);
        
        runtime = ScriptRuntime.Create();
        runtime.LoadAssembly(typeof(string).Assembly);
        //runtime.LoadAssembly(typeof(Debug).Assembly);
        engine = runtime.GetEngine("py");

        Encoding encoding = Encoding.UTF8;
        //engine.Runtime.IO.SetInput(stdin, encoding);
        engine.Runtime.IO.SetOutput(output_stream, encoding);
        engine.Runtime.IO.SetErrorOutput(output_stream, encoding);
        scope = engine.Runtime.CreateScope();
        
        // Probably a more direct way to do these things:
        command("import sys");
        command("sys.path.append('.')");
        command("sys.path.append('python')");
        command("sys.path.append('DLLs')");
        buffer.Text += "# Python " + command("sys.version") + "\n" +
                       "# " + command("sys.copyright") + "\n";
        command("del sys");
        
        source_view.AddPrompt();
    }

    // Evaluates an expression or executes a statement.
    // Call this for user command-line input when you don't know if
    // the string is an expression or statement.
    public override string command(string line)
    {
        string retval = "";
        try {
            // FIXME: do the conversion to REPR in C# (replace ToString())
            object o = engine.CreateScriptSourceFromString("repr(" + line + ")", 
                                 SourceCodeKind.Expression).Execute(scope);
            if (o != null) {
                retval = o.ToString();
            }
        } catch (Exception) {
            // FIXME: what is the IP2 version of PythonSyntaxErrorException?
            // Not a valid expression, so try it as a statement
            try {
                engine.CreateScriptSourceFromString(line, 
                            SourceCodeKind.Statements).Execute(scope);
            } catch (Exception e) {
                retval = e.ToString();
            }
        }
        return retval;
    }

}

