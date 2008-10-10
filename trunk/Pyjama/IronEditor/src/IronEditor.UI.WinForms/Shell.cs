using System;
using System.IO;
using System.Collections.Generic;
using System.Threading;


abstract public class BaseShell {
  //protected SourceBuffer buffer;
  //protected SourceLanguage language;
  //protected ShellSourceView source_view;
    protected string command_line;
    protected string languageName;
    protected string languageMime;
  //protected SourceLanguagesManager mgr;
    
    public BaseShell()
    {
    }
    
    public void setProgrammingLanguage(string languageName, 
				       string languageMime) {
	this.languageName = languageName;
	this.languageMime = languageMime;
    }
    
    public abstract string command(string line);

    void do_exec()
    {
        // Evaluate or execute
	  //source_view.AppendText(command(command_line));
        
        // Don't give a newline unless we need one:
//         if (iter.CharsInLine != 0) {
//             source_view.AppendText("\n");
//         }
//         source_view.AddPrompt();
    }

    protected void execute(string line)
    {
        //FIXME - Prevent editing the ShellSourceView while the command is running.
        //      Also prevent execute from being called again.
        //FIXME - Gtk is probably not threeadsafe, so we need a mutex for python stdout.
        command_line = line;
        do_exec();
        /*
        Thread thread = new Thread(this.do_exec);
        thread.Start();
        
        
        thread.Join();
        
        // Don't give a newline unless we need one:
        int cursor_pos = buffer.CursorPosition;
        TextIter iter = buffer.GetIterAtOffset(cursor_pos);
        if (iter.CharsInLine != 0) {
            source_view.AppendText("\n");
        }*/
    }

    public void ExecuteFile(string filename)
    {
        // FIXME: import file
    }

    public void EvaluateExp(string expression)
    {
        // FIXME: evaluate exp in env
    }

    public void Restart()
    {
        // Fixme: restart the shell (with new env)
    }

    public void Quit()
    {
        // FIXME: close the shell
    }
    
  public void Print() {
	//PrintText print = new PrintText(source_view);
  }

  //    public Widget GetView()
  //{
  //return (Widget) source_view;
  //}
}

