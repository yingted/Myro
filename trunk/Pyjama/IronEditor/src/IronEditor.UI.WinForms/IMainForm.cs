using System.IO;
using IronEditor.UI.WinForms.Controls;

namespace IronEditor.UI.WinForms
{
    public interface IMainForm
    {
        TextWriter GetOutputStream();
        CodeBlock GetCodeBlock();
        void PrintConsoleMessage(string message);
        void PrintLineConsoleMessage(string message);
        void PrintPrompt();
        void OpenFile(ActiveCodeFile code);
        void ClearOutputStream();
        void ClearOpenFiles();
        bool HasFileOpen { get; }
        ActiveCodeFile GetCurrentActiveFile();
        void SetSaveInformationForActiveFile(string location);
    }
}
