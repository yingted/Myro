using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Microsoft.Win32;
using System.IO;
using Myro.Utilities;

namespace Myro.GUI.SimpleIDE
{
    public class Editor
    {
        /// <summary>
        /// Contains information about a document.
        /// </summary>
        public class EditorDocument
        {
            public string FileName { get; internal set; }
            public string FullName { get; internal set; }
            public bool HasLocation { get; internal set; }
            public bool IsModified { get; internal set; }
            public TextBox EditorControl { get; internal set; }
            public EditorDocument()
            {
                FileName = null;
                FullName = null;
                HasLocation = false;
                IsModified = false;
                EditorControl = null;
            }
        }

        #region Events

        /// <summary>
        /// Event arguments, containing the Document object affected by the
        /// event.
        /// </summary>
        public class EditorEventArgs : EventArgs
        {
            public EditorDocument Document { get; internal set; }
        }

        public delegate void EditorEventHandler(object sender, EditorEventArgs e);

        /// <summary>
        /// This event fires when a new editor is created, as the result of a 
        /// "new" or "open" operation.
        /// </summary>
        public event EditorEventHandler InsertedEditor;

        /// <summary>
        /// This event fires when an editor is closed.
        /// </summary>
        public event EditorEventHandler RemovedEditor;

        /// <summary>
        /// This event fires when an editor name or path changes, as the result
        /// of a first-time save or "save as".
        /// </summary>
        public event EditorEventHandler NameChanged;

        /// <summary>
        /// This event fires when an unmodified editor is modified, or a 
        /// modified editor is saved.
        /// </summary>
        public event EditorEventHandler ModifiedChanged;

        /// <summary>
        /// This event fires when a document should be activated, such as when
        /// prompting to save the document.
        /// </summary>
        public event EditorEventHandler ActivatedEditor;

        #endregion Events

        #region Properties

        private List<EditorDocument> documents = new List<EditorDocument>();
        /// <summary>
        /// A read-only list of the current documents.
        /// </summary>
        public IList<EditorDocument> Documents
        {
            get
            {
                return documents.AsReadOnly();
            }
        }

        #endregion

        #region Private members

        private int untitledNumer = 1;
        private Window owner = null;
        private const string fileFilter = "Python files (.py)|*.py|All files|*";

        #endregion

        /// <summary>
        /// Constructor.  You should hook up event handlers after calling this
        /// so your view is updated.
        /// </summary>
        public Editor(Window owner)
        {
            this.owner = owner;
        }

        /// <summary>
        /// Create a new document.  The InsertedEditor event will fire as a
        /// result of this call.
        /// </summary>
        public void RequestNewDocument()
        {
            RequestNewDocument(new EditorDocument(), "");
        }

        public void RequestNewDocument(EditorDocument template, String text)
        {
            if (template.FullName != null && template.FullName.Length > 0)
            {
                template.HasLocation = true;
                template.FileName = System.IO.Path.GetFileName(template.FullName);
            }
            else
            {
                template.HasLocation = false;
                if (template.FileName == null || template.FileName.Length <= 0)
                {
                    template.FileName = "Untitled" + untitledNumer;
                    untitledNumer++;
                }
            }
            template.EditorControl = new TextBox()
                {
                    AcceptsReturn = true,
                    AcceptsTab = true,
                    FontFamily = new FontFamily("Courier New"),
                    FontSize = 12.0,
                    HorizontalScrollBarVisibility = ScrollBarVisibility.Auto,
                    VerticalScrollBarVisibility = ScrollBarVisibility.Auto,
                    Text = text,
                    SnapsToDevicePixels = true,
                    //BorderThickness = new Thickness(0,2,0,0),
                    Margin = new Thickness(0),
                    Padding = new Thickness(0)
                };
            template.EditorControl.TextChanged +=
                delegate(object source, TextChangedEventArgs e)
                {
                    if (template.IsModified == false)
                    {
                        template.IsModified = true;
                        ModifiedChanged.Invoke(this, new EditorEventArgs() { Document = template });
                    }
                };

            documents.Add(template);

            InsertedEditor.Invoke(this, new EditorEventArgs() { Document = template });
        }

        /// <summary>
        /// Save a document.  Takes care of prompting for a file name, etc.
        /// Throws an Exception if saving the file causes one.  (But also pops
        /// up a message box alerting the user.)
        /// </summary>
        /// <param name="document"></param>
        /// <returns>True if the file was saved successfully or was not modified.
        /// False if the user cancels.</returns>
        public bool RequestSaveDocument(EditorDocument document)
        {
            if (documents.Contains(document))
            {
                // Choose location if first save.  Also sets IsModified to true.
                bool cancelled;
                bool locationChosen;
                if (!document.HasLocation)
                {
                    ActivatedEditor.Invoke(this, new EditorEventArgs() { Document = document });

                    var dlg = new SaveFileDialog()
                    {
                        DefaultExt = ".py",
                        Filter = fileFilter
                    };
                    if (dlg.ShowDialog(this.owner) == true)
                    {
                        document.FullName = dlg.FileName;
                        document.HasLocation = true;
                        document.IsModified = true;
                        cancelled = false;
                    }
                    else
                        cancelled = true;
                    locationChosen = true;
                }
                else
                {
                    cancelled = false;
                    locationChosen = false;
                }

                // If selecting a path was not cancelled, and document is modified
                // (IsModified was set to true if the file did not have a path).
                if (cancelled == true)
                    return false;
                else
                {
                    if (document.IsModified)
                    {
                        try
                        {
                            using (var stream = File.CreateText(document.FullName))
                            {
                                stream.Write(document.EditorControl.Text);
                                document.IsModified = false;
                                if (locationChosen)
                                    NameChanged.Invoke(this, new EditorEventArgs() { Document = document });
                                ModifiedChanged.Invoke(this, new EditorEventArgs() { Document = document });
                            }
                        }
                        catch (Exception e)
                        {
                            if (locationChosen)
                                document.HasLocation = false;
                            if (e.Message != null && e.Message.Length > 0)
                                MessageBox.Show(this.owner, e.Message, "Error saving file",
                                    MessageBoxButton.OK, MessageBoxImage.Error, MessageBoxResult.OK);
                        }
                    }
                    return true;
                }
            }
            else
            {
                throw new ArgumentException("This document is not one of the editor's open documents");
            }
        }

        /// <summary>
        /// Prompt the user to save the document if it is unsaved.  Throws
        /// an Exception if saving the file throws an exception.
        /// </summary>
        /// <param name="document"></param>
        /// <returns>True if the file was saved, not modified, or the user did not
        /// want to save it.  False if the user cancelled the operation.</returns>
        public bool RequestSaveWithPrompt(EditorDocument document)
        {
            if (document.IsModified || !document.HasLocation)
                switch (MessageBox.Show(this.owner, Strings.SavePrompt(document.FileName), "Myro",
                    MessageBoxButton.YesNoCancel, MessageBoxImage.Question, MessageBoxResult.Cancel))
                {
                    case MessageBoxResult.Yes:
                        if (RequestSaveDocument(document) == false)
                            return false;
                        else
                            return true;
                    case MessageBoxResult.No:
                        return true;
                    case MessageBoxResult.Cancel:
                        return false;
                    default:
                        return false;
                }
            else
                return true;
        }

        /// <summary>
        /// Save all documents.  Throws an Exception if saving a file causes an exception.
        /// </summary>
        /// <returns>True if all files were saved successfully or not modified.
        /// False if the user cancels the operation.</returns>
        public bool RequestSaveAll()
        {
            foreach (var document in documents)
                if (RequestSaveDocument(document) == false)
                    return false;
            return true;
        }

        /// <summary>
        /// Prompts the user to save all unsaved documents.  Throws an Exception
        /// if saving the file causes an exception.
        /// </summary>
        /// <returns>True if all documents were saved or the user specified not to save them.
        /// False if the user cancelled the operation.</returns>
        public bool RequestSaveAllWithPrompt()
        {
            foreach (var document in documents)
            {
                if (RequestSaveWithPrompt(document) == false)
                    return false;
            }
            return true;
        }

        /// <summary>
        /// Opens a file from disk.  Throws an Exception if reading the file
        /// causes an exception.
        /// </summary>
        /// <returns>True if the file is loaded successfully, or False if the
        /// user cancells the operation.</returns>
        public bool RequestOpen()
        {
            var dlg = new OpenFileDialog()
            {
                DefaultExt = ".py",
                Filter = fileFilter
            };
            if (dlg.ShowDialog(this.owner) == true)
            {
                using (var stream = File.OpenText(dlg.FileName))
                {
                    RequestNewDocument(new EditorDocument()
                    {
                        FileName = dlg.SafeFileName,
                        FullName = dlg.FileName
                    },
                        stream.ReadToEnd());
                }
                return true;
            }
            else
                return false;
        }

        /// <summary>
        /// Closes a document, prompting to save if necessary.  Throws an 
        /// Exception if saving causes an Exception.
        /// </summary>
        /// <param name="document"></param>
        /// <returns>True if the document was closed, false if the user cancelled
        /// the operation.</returns>
        public bool RequestClose(EditorDocument document)
        {
            if (RequestSaveWithPrompt(document) == false)
                return false;
            else
            {
                RemovedEditor.Invoke(this, new EditorEventArgs() { Document = document });
                return true;
            }
        }

        /// <summary>
        /// Closes all documents, prompting to save if necessary.  Throws an 
        /// Exception if saving causes an Exception.
        /// </summary>
        /// <returns>True if all documents were closed, false if the user cancelled
        /// the operation.</returns>
        public bool RequestCloseAll()
        {
            foreach (EditorDocument document in documents)
                if (RequestClose(document) == false)
                    return false;
            return true;
        }
    }
}
