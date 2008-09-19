using System;

namespace IronEditor.UI.WinForms.Controls
{
    public class IDETextBoxEvent
    {
        public delegate void TextChangedHandler(object sender, EventArgs e);
    }

    public interface IIDETextBox
    {
        string Code { get; set; }
        string FontName { get; set; }
        float FontSize { get; set; }
        string SelectedText { get; }
        void Undo();
        void Cut();
        void Copy();
        void Paste();
        void SelectAll();
        event IDETextBoxEvent.TextChangedHandler TextChanged;

    }
}