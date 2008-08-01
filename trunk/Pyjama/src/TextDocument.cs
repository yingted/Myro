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

using System.IO;
using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Drawing;
using PyjamaInterfaces;

public class TextDocument: TextBox, PyjamaInterfaces.IDocument {
  string filename;
  int page;
  bool untitled;
  string mime_type;
  PyjamaForm mainForm;

  public TextDocument(PyjamaForm form, string filename, int page) {
	mainForm = form;
	this.filename = filename;
	this.page = page;
	untitled = true;
	mime_type = GetMimeType(this.filename);

	// Windows Forms Settings

	// Optional:
	WordWrap = true;
	setFont();

	// System:
 	Multiline = true;
	AcceptsTab = true;
	AcceptsReturn = true;
	ScrollBars = ScrollBars.Both;
	Dock = DockStyle.Fill;
		
	// 	AllowDrop = true;
	// 	Location = new System.Drawing.Point(28, 129);
	// 	DragDrop += new DragEventHandler(TextBox_DragDrop);
	// 	DragEnter += new DragEventHandler(TextBox_DragEnter);
	// 	MouseDown += new MouseEventHandler(TextBox_MouseDown);

	if (filename != null && File.Exists(this.filename)) {
	  StreamReader file = File.OpenText(this.filename);
	  //buffer.BeginNotUndoableAction();
	  Text = file.ReadToEnd();
	  //buffer.EndNotUndoableAction();
	  //buffer.PlaceCursor(buffer.StartIter);
	  untitled = false;
	  file.Close();
	} else {
	  filename = Utils.Tran("Untitled") + "-" + (this.page + 1) + ".py";
	  untitled = true;
	}
  }

  public void setFont() {
	this.Font = new Font("Courier New", 18.0f,
		FontStyle.Regular, GraphicsUnit.Pixel);
  }

  public void setFont(string fontName, float size, FontStyle style) {
	Font testFont = new Font(fontName, size, style, GraphicsUnit.Pixel);
	if (testFont.Name == fontName) {
	  this.Font = testFont;
	} else {
	  setFont();
	}
  }

  private void TextBox_MouseDown(object sender, MouseEventArgs e) {
	TextBox txt = (TextBox)sender;
	txt.SelectAll();
	txt.DoDragDrop(txt.Text, DragDropEffects.Copy);
  }
  
  private void TextBox_DragEnter(object sender, DragEventArgs e) {
	if (e.Data.GetDataPresent(DataFormats.Text)) {
	  e.Effect = DragDropEffects.Copy;
	} else {
	  e.Effect = DragDropEffects.None;
	}
  }
  
  private void TextBox_DragDrop(object sender, DragEventArgs e) {
	TextBox txt = (TextBox)sender;
	txt.Text = (string)e.Data.GetData(DataFormats.Text);
  }
  
  protected override bool ProcessCmdKey(ref 
	  Message m, 
	  Keys k) 
  {
	// Need Customizable key bindings
	if(m.Msg == 256) {
	  // Control + keys
	  if (k == (Keys.Control | Keys.E)) {
		SendKeys.SendWait("{END}");
		return true; 
	  } else {
		// Plain keys
		if (k == Keys.Tab) {
		  SendKeys.SendWait("    ");
		  return true; 
		} else if (k == Keys.Return) {
		  // does not get rehandled:
		  SendKeys.SendWait("{ENTER}    ");
		  return true; 
		}
	  }
	}
	return base.ProcessCmdKey(ref m,k);
  }

	//SourceLanguagesManager mgr = new SourceLanguagesManager();
	// mgr.LangFilesDirs.Append("./language-specs");
	//source_view = new SourceView();
// 	if (mime_type != "text/plain") {
// 	  language = mgr.GetLanguageFromMimeType(mime_type);
// 	  //buffer = new SourceBuffer(language);
// 	  buffer.Highlight = true;
// 	  source_view.Buffer = buffer;
// 	  // Options should be set by user:
// 	  source_view.WrapMode = Gtk.WrapMode.Word;
// 	  source_view.ShowLineNumbers = true;
// 	  source_view.AutoIndent = true;
// 	} else {
// 	  // Plain text doesn't need any highlighting.
// 	  // I think this causes a Gtk assert issue if not set.
// 	  language = mgr.GetLanguageFromMimeType("text/x-changelog");
// 	  buffer = new SourceBuffer(language);
// 	  buffer.Highlight = false;
// 	  source_view.Buffer = buffer;
// 	  // Options should be set by user:
// 	  source_view.WrapMode = Gtk.WrapMode.Word;
// 	  source_view.ShowLineNumbers = false;
// 	  source_view.AutoIndent = false;
// 	}
// 	// Change the font to fixed-width:
// 	Pango.FontDescription font = new Pango.FontDescription();
// 	font.Family = "Monospace";
// 	// FIXME: This may work differently on different platforms
// 	// value is in points * PangoScale
// 	// Pango also has 11 predefined sizes
// 	font.AbsoluteSize = 15 * Pango.Scale.PangoScale;
// 	source_view.ModifyFont(font);
	
// 	// Set up event handlers:
// 	source_view.Buffer.Changed += new EventHandler(OnSourceViewChanged);
// 	source_view.KeyPressEvent += new KeyPressEventHandler(OnKeyPressBefore);
	
  
//   private void OnKeyPressBefore(object obj, KeyPressEventArgs args) 
//     {
// 	// FIXME:
// 	// This isn't correct as they should happen after the change
// 	// but source_view.KeyPressEvent eats the signal, and won't
// 	// call any other events, and CanUndoFired crashes Windows
// 	gui.SetDirty(page, buffer.CanUndo());
// 	source_view.ScrollMarkOnscreen(buffer.InsertMark);
//     }

//     private void OnSourceViewChanged(object obj, EventArgs args) 
//     {
// 	gui.SetDirty(page, buffer.CanUndo());
// 	source_view.ScrollMarkOnscreen(buffer.InsertMark);
//     }
    
//     public bool Untitled
//     {
//         get
//         {
//             return untitled;
//         }
//     }
    
//     public Widget GetView()
//     {
// 	return (Widget) source_view;
//     }

  public string GetShortName()
  {
 	// Path.GetDirectoryName()
 	string name = System.IO.Path.GetFileName(filename);
 	if (name == "__init__.py") {
	  // add directory onto name
	  char [] c = new char[1];
	  c[0] = Path.DirectorySeparatorChar;
	  string [] path = filename.Split(c,
		  StringSplitOptions.RemoveEmptyEntries);
	  if (path.Length > 1)
 		name = path[path.Length - 2] + Path.DirectorySeparatorChar + name;
 	}
 	return name;
  }
    
  public bool GetModified()
  {
	return Modified;
  }
        
  public void SetModified(bool value)
  {
	Modified = value;
  }

  public void Save() {
	StreamWriter file = new StreamWriter(filename);
	file.Write(Text);
	file.Close();
	Modified = false;
// 	gui.SetDirty(page, false);
// 	// Forces no redo past this point
// 	//buffer.BeginNotUndoableAction(); 
// 	//buffer.EndNotUndoableAction();
  }
  
  public void SaveAs(string filename) {
	// 	if (File.Exists(fn)) {
	//             MessageDialog dlg = new MessageDialog(gui.MainWindow, DialogFlags.Modal,
	//                 MessageType.Question, ButtonsType.None,
	//                 "Do you want to overwrite \"{0}\"?", fn);
	//             dlg.AddButton("Overwrite", ResponseType.Accept);
	//             dlg.AddButton("Cancel", ResponseType.Cancel);
	//             ResponseType ret = (ResponseType)dlg.Run();
	//             dlg.Destroy();
	//             if (ret == ResponseType.Cancel) {
	// 		return;
	// 	    }
	// 	} 
	// 	untitled = false;
	// 	filename = fn;
	// 	string mime_type = GetMimeType(filename);
	// 	SourceLanguagesManager mgr = new SourceLanguagesManager();
	// 	language = mgr.GetLanguageFromMimeType(mime_type);
	// 	if (buffer.Language != language && language != null) {
	// 	    buffer.Language = language;
	// 	}
	// 	Save();
  }
  
  public void SetFilename(string filename)
  {
	this.filename = filename;
  }
  
  public string GetFilename()
  {
	return filename;
  }
  
  string GetMimeType(string filename) {
	if (filename != null) {
	  string extension = System.IO.Path.GetExtension(filename);
	  return Utils.GetMimeType(extension);
	} else {
	  return "text/x-python";
	}
  }
  
  public bool GetDirty() {
	//return buffer.CanUndo();
	return true;
	
  }
  
  public int GetPage() {
	return page;
  }
  
  public void SetPage(int value) {
	page = value;
  }
  
  public int GetSize() {
	//return buffer.CharCount;
	return 0;
  }
  
  //     public TextBuffer Buffer
  //     {
  //         get
  //         {
  //             return buffer;
  //         }
  //     }
  
  public void Cut()
  {
	//         Clipboard clipboard = Clipboard.Get(Gdk.Atom.Intern("CLIPBOARD", true));
	//         buffer.CutClipboard(clipboard, true);
  }
  
  public void Copy()
  {
	//         Clipboard clipboard = Clipboard.Get(Gdk.Atom.Intern("CLIPBOARD", true));
	//         buffer.CopyClipboard(clipboard);
  }
  
  public void Paste()
  {
	//         Clipboard clipboard = Clipboard.Get(Gdk.Atom.Intern("CLIPBOARD", true));
	//         buffer.PasteClipboard(clipboard);
  }
  
  public void Delete()
  {
	//         buffer.DeleteSelection(true, true);
  }
  
  public void Print()
  {
	//         PrintText print = new PrintText(source_view);
  }
}
