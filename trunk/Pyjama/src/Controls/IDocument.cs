using System;

namespace Pyjama
{
    public interface IDocument
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
        event Document.TextChangedHandler TextChanged;
    }
}