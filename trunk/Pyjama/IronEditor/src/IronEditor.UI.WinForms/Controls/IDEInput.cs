using System;
using System.Drawing;

namespace IronEditor.UI.WinForms.Controls
{
    public class IDEInput
    {
        private IIDETextBox IDETextBox;

        public IDEInput(IIDETextBox ideTextBox)
        {
            IDETextBox = ideTextBox;
        }

        public string Text
        {
            get { return IDETextBox.Code; }
            set { IDETextBox.Code = value; }
        }

        public Font Font
        {
            get { return new Font(IDETextBox.FontName, IDETextBox.FontSize); }
            set
            {
                IDETextBox.FontName = value.FontFamily.Name;
                IDETextBox.FontSize = value.Size;
            }
        }

        public string SelectedText
        {
            get { return IDETextBox.SelectedText; }
        }

        public void Undo()
        {
            if (IDETextBox != null)
                IDETextBox.Undo();
        }

        public void Cut()
        {
            if (IDETextBox != null)
                IDETextBox.Cut();
        }

        public void Copy()
        {
            if (IDETextBox != null)
                IDETextBox.Copy();
        }

        public void Paste()
        {
            if (IDETextBox != null)
                IDETextBox.Paste();
        }

        public void SelectAll()
        {
            if (IDETextBox != null)
                IDETextBox.SelectAll();
        }

    }
}