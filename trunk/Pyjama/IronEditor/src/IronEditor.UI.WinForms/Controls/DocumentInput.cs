using System;
using System.Drawing;

namespace Pyjama
{
    public class DocumentInput
    {
        private IDocument documentTextBox;

        public DocumentInput(IDocument ideTextBox)
        {
            documentTextBox = ideTextBox;
        }

        public string Text
        {
            get { return documentTextBox.Code; }
            set { documentTextBox.Code = value; }
        }

        public Font Font
        {
            get { return new Font(documentTextBox.FontName, documentTextBox.FontSize); }
            set
            {
                documentTextBox.FontName = value.FontFamily.Name;
                documentTextBox.FontSize = value.Size;
            }
        }

        public string SelectedText
        {
            get { return documentTextBox.SelectedText; }
        }

        public void Undo()
        {
            if (documentTextBox != null)
                documentTextBox.Undo();
        }

        public void Cut()
        {
            if (documentTextBox != null)
                documentTextBox.Cut();
        }

        public void Copy()
        {
            if (documentTextBox != null)
                documentTextBox.Copy();
        }

        public void Paste()
        {
            if (documentTextBox != null)
                documentTextBox.Paste();
        }

        public void SelectAll()
        {
            if (documentTextBox != null)
                documentTextBox.SelectAll();
        }

    }
}