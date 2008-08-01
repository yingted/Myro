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
using System.Drawing;
using PyjamaInterfaces;

public class PyjamaForm : Form {
  List <IDocument> documents = new List <IDocument>();
  private TabControl notebook;
  internal StatusBar statusBar1;
  
  public PyjamaForm(string[] args) {
	Text = "Pyjama Project";
	setFontFinal();
	InitializeComponent();
	InitializeStatusBarPanels();
  }

  private void InitializeComponent() {
	notebook = new System.Windows.Forms.TabControl();

	TabPage newPage = new TabPage("Untitled");	
	notebook.TabPages.Add(newPage);	
	notebook.Dock = DockStyle.Top;
	notebook.Height = 240; //this.Height / 2;
	notebook.TabIndex = 0;
	TextDocument textBox = new TextDocument(this, "Untitled", 0);
	newPage.Controls.Add(textBox);

	Splitter splitter = new Splitter();
	splitter.BorderStyle = BorderStyle.FixedSingle;
	//splitter.BackColor = Color.Red;
	//splitter.Location = new Point (0, 240);
	//splitter.Size = new Size(0, 7);
	splitter.TabStop = false;
	splitter.Dock = DockStyle.Top;
	//splitter.Size = new System.Drawing.Size(3, 273);
	//splitter.BackColor = Color.LightGray;
	splitter.Height = 6;

	TextBox shell = new TextBox();
	shell.WordWrap = true;
 	shell.Multiline = true;
	shell.AcceptsTab = true;
	shell.AcceptsReturn = true;
	shell.ScrollBars = ScrollBars.Both;
	shell.Dock = DockStyle.Fill;

	SuspendLayout();
	AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
	ClientSize = new System.Drawing.Size(640, 480);

	Controls.Add(splitter);
	Controls.Add(notebook);
	Controls.Add(shell);

	ResumeLayout(false);
	PerformLayout();
  }

  private void InitializeStatusBarPanels() {
    statusBar1 = new StatusBar();
    statusBar1.Dock = DockStyle.Bottom;
    statusBar1.SizingGrip = true;
    statusBar1.PanelClick += 
        new StatusBarPanelClickEventHandler(statusBar1_PanelClick);

    // Create two StatusBarPanel objects to display in statusBar1.
    StatusBarPanel panel1 = new StatusBarPanel();
    StatusBarPanel panel2 = new StatusBarPanel();

    // Set the width of panel2 explicitly and set
    // panel1 to fill in the remaining space.
    panel2.Width = 80;
    panel1.AutoSize = StatusBarPanelAutoSize.Spring;

    // Set the text alignment within each panel.
    panel1.Alignment = HorizontalAlignment.Left;
    panel2.Alignment = HorizontalAlignment.Right;

    // Display the first panel without a border and the second
    // with a raised border.
    panel1.BorderStyle = StatusBarPanelBorderStyle.None;
    panel2.BorderStyle = StatusBarPanelBorderStyle.Raised;

    // Set the text of the panels. The panel1 object is reserved
    // for line numbers, while panel2 is set to the current time.
    panel1.Text = "Reserved for important information.";
    panel2.Text = System.DateTime.Now.ToShortTimeString();

    // Set a tooltip for panel2
    panel2.ToolTipText = "Click time to display seconds";

    // Display panels in statusBar1 and add them to the
    // status bar's StatusBarPanelCollection.
    statusBar1.ShowPanels = true;
    statusBar1.Panels.Add(panel1);
    statusBar1.Panels.Add(panel2);

    // Add the StatusBar to the form.
    this.Controls.Add(statusBar1);
  }


  // If the user clicks the status bar, check the text of the 
  // StatusBarPanel.  If the text equals a short time string,
  // change it to long time display.
  private void statusBar1_PanelClick(object sender, 
	  StatusBarPanelClickEventArgs e)
  {
    if (e.StatusBarPanel.Text == 
        System.DateTime.Now.ToShortTimeString())
    {
	  e.StatusBarPanel.Text = 
		  System.DateTime.Now.ToLongTimeString();
    }
  }

  public void setFontFinal() {
	// Options:
  }

  public void AddFile(string filename) {
	IDocument textdoc;
	//int page = notebook.NPages;
	// FIXME: select what type of IDocument to create:
	textdoc = new TextDocument(this, filename, 0);
	// FIXME: why does the cursor not stay in view?
	//ScrolledWindow sw = new ScrolledWindow();
	//sw.Add(textdoc.GetView());
	//sw.HscrollbarPolicy = PolicyType.Automatic;
	//sw.VscrollbarPolicy = PolicyType.Automatic;
	//notebook.AppendPage(sw, new Label(textdoc.GetShortName()));
	//documents.Add(textdoc);
	//notebook.ShowAll();
	//notebook.Page = notebook.NPages - 1;
	//textdoc.GetView().GrabFocus();
  }
    
  [STAThread] // for windows interop with clipboard, etc
  public static void Main(string[] args) {
	PyjamaForm form = new PyjamaForm(args);
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
        dlg.Name = "Pyjama";
        dlg.Run();
        dlg.Destroy();
    }
}
	*/
