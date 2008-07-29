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

using System;
using System.IO;
using System.Collections.Generic;

using System.Windows.Forms;

using PyjamaInterfaces;

namespace PyjamaGUI {
  /*
  [Widget] Window main_window;
  [Widget] Container shell_container;
  [Widget] Notebook notebook;
  */
    
  public class PyjamaForm : Form {
	List <IDocument> documents = new List <IDocument>();

	public PyjamaForm(string[] args) {
	  this.Text = "Pyjama Project";
	}
	
	[STAThread] // for windows interop with clipboard, etc
	public static void Main(string[] args) {
	  
	  PyjamaForm form = new PyjamaForm(args);
	  
	}
	
	
  }
  
  
}

	/*
       main_window.DeleteEvent += File_Exit;
        
        if (shell_container != null)
        {
            IShell shell = new PythonShell();
            shell_container.Add(shell.GetView());
        }
        
        // Open files on the command line
        if (args.Length > 0)
        {
            foreach (string name in args)
            {
                AddFile(name);
            }
        } else {
            // Add a blank page:
            AddFile(null);
        }
    }
    
    public Window MainWindow
    {
        get { return main_window; }
    }
    
    public void ShowAll()
    {
        main_window.ShowAll();
    }
    
    public void AddFile(string filename)
    {
        IDocument textdoc;
        int page = notebook.NPages;
        // FIXME: select what type of IDocument to create:
        textdoc = new TextDocument(this, filename, page);
        // FIXME: why does the cursor not stay in view?
        ScrolledWindow sw = new ScrolledWindow();
        sw.Add(textdoc.GetView());
        sw.HscrollbarPolicy = PolicyType.Automatic;
        sw.VscrollbarPolicy = PolicyType.Automatic;
        notebook.AppendPage(sw, new Label(textdoc.GetShortName()));
        documents.Add(textdoc);
        notebook.ShowAll();
        notebook.Page = notebook.NPages - 1;
        textdoc.GetView().GrabFocus();
    }
    
    public void SetDirty(int page, bool dirty)
    {
        Widget widget = notebook.GetNthPage(page);
        if (widget != null) {
            string labelText = notebook.GetTabLabelText(widget);
            //FIXME - Use a real flag.  Filenames can actually start with '*'.
            if (dirty) {
                if (!labelText.StartsWith("*")) {
                    notebook.SetTabLabelText(widget, "*" + labelText);
                }
            } else {
                if (labelText.StartsWith("*")) {
                    notebook.SetTabLabelText(widget, labelText.Substring(1, 
                                                          labelText.Length - 1));
                }
            }
        }
    }

    // Returns the document that is currently selected in the file list
    public IDocument CurrentDocument
    {
        get
        {
            int page_num = notebook.CurrentPage;
            return documents[page_num];
        }
    }
    
    public void File_New(object obj, EventArgs args)
    {
        File_New();
    }
    
    public void File_New()
    {
        AddFile(null);
    }

    public void File_Open(object obj, EventArgs args)
    {
        File_Open();
    }
    
    public void File_Open()
    {
        FileChooserDialog dlg = new FileChooserDialog(
                                       "Open", main_window, FileChooserAction.Open,
                                       "Cancel", ResponseType.Cancel,
                                       "Open", ResponseType.Accept);
        if (dlg.Run() == (int)ResponseType.Accept)
        {
            // Open the file
            //FIXME - Error handling
            if (notebook.NPages > 0)
            {
                IDocument last_doc = documents[notebook.NPages - 1];
                if (!last_doc.GetDirty() && last_doc.Untitled && last_doc.GetSize() == 0)
                {
                    int page = notebook.NPages - 1;
                    notebook.RemovePage(page);
                    foreach (IDocument doc in documents){
                        if (doc.GetPage() > page) {
                            doc.SetPage(doc.GetPage() - 1);
                        }
                    }
                    documents.RemoveAt(page);
                }
            }
            AddFile(dlg.Filename);
        }
        dlg.Destroy();
    }
    
    public void File_Save(object obj, EventArgs args)
    {
        File_Save();
    }
    
    public bool File_Save()
    {
        IDocument doc = CurrentDocument;
        if (doc.Untitled)
        {
            return File_Save_As();
        } else {
            doc.Save();
            return true;
        }
    }
    
    public void File_Save_As(object obj, EventArgs args)
    {
        File_Save_As();
    }
    
    public bool File_Save_As()
    {
        IDocument doc = CurrentDocument;
        FileChooserDialog dlg = new FileChooserDialog("Save As", main_window, 
                                                      FileChooserAction.Save,
                                                      "Cancel", ResponseType.Cancel,
                                                      "Save", ResponseType.Accept);
        bool ret = (dlg.Run() == (int)ResponseType.Accept);
        if (ret)
        {
            doc.SaveAs(dlg.Filename);
            notebook.SetTabLabelText(notebook.CurrentPageWidget, 
                                     doc.GetShortName());
        }
        dlg.Destroy();
        
        return ret;
    }
    
    public void File_Close(object obj, EventArgs args)
    {
        IDocument currentdoc = CurrentDocument;
        if (currentdoc.GetDirty())
        {
            // Save/Discard/Cancel prompt
            MessageDialog dlg = new MessageDialog(main_window, DialogFlags.Modal,
                MessageType.Question, ButtonsType.None,
                "The document \"{0}\" has been modified.\nDo you want to save or discard changes?",
                currentdoc.GetFilename());
            dlg.AddButton("Save", ResponseType.Accept);
            dlg.AddButton("Discard", ResponseType.Reject);
            dlg.AddButton("Cancel", ResponseType.Cancel);
            ResponseType ret = (ResponseType)dlg.Run();
            dlg.Destroy();
            
            if ((ret == ResponseType.Accept && !File_Save()) || ret == ResponseType.Cancel || ret == ResponseType.DeleteEvent)
            {
                // Save failed or cancelled
                return;
            }
        }
        
        int page_num = notebook.CurrentPage;
        notebook.RemovePage(page_num);
        foreach (IDocument doc in documents){
            if (doc.GetPage() > page_num) {
                doc.SetPage(doc.GetPage() - 1);
            }
        }
        documents.RemoveAt(page_num);
        // This would prevent an empty page
        //if (notebook.NPages == 0)
        //    add_file(null);
        notebook.Show();
    }
    
    public void File_Exit(object obj, EventArgs args)
    {
        File_Exit();
    }
    

    public void File_Exit()
    {
        //FIXME - Ask to abandon changes on all modified documents
        Application.Quit();
    }
    
    public void File_Print(object obj, EventArgs args)
    {
        File_Print();
    }
    
    public void File_Print()
    {
        CurrentDocument.Print();
    }
    
    public void Edit_Cut(object obj, EventArgs args)
    {
        Edit_Cut();
    }
    
    public void Edit_Cut()
    {
        CurrentDocument.Cut();
    }
    
    public void Edit_Copy(object obj, EventArgs args)
    {
        Edit_Copy();
    }
    
    public void Edit_Copy()
    {
        CurrentDocument.Copy();
    }
    
    public void Edit_Paste(object obj, EventArgs args)
    {
        Edit_Paste();
    }
    
    public void Edit_Paste()
    {
        CurrentDocument.Paste();
    }
    
    public void Edit_Delete(object obj, EventArgs args)
    {
        Edit_Delete();
    }
    
    public void Edit_Delete()
    {
        CurrentDocument.Delete();
    }
    
    public void Edit_Find(object obj, EventArgs args)
    {
        Edit_Find();
    }
    
    public void Edit_Find()
    {
        find.Run(CurrentDocument.Buffer);
    }
    
    public void Edit_Replace(object obj, EventArgs args)
    {
        Edit_Replace();
    }
    
    public void Edit_Replace()
    {
    }
    
    public void Help_About(object obj, EventArgs args)
    {
        Help_About();
    }
    
    public void Help_About()
    {
        AboutDialog dlg = new AboutDialog();
        //FIXME - Real information
	//FIXME - we should be targeting Mono >= 1.9
#if GTKVER1
        dlg.ProgramName = "Pyjama";
#else        
        dlg.Name = "Pyjama";
#endif
        dlg.Run();
        dlg.Destroy();
    }
}
	*/
