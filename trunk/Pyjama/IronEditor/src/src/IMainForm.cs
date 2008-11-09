using System.IO;
using Pyjama;

namespace Pyjama
{
    public interface IMainForm
    {
        TextWriter GetOutputStream();
        CodeBlock GetCodeBlock();
        void PrintConsoleMessage(string message);
        void PrintLineConsoleMessage(string message);
        void PrintPrompt();
        void OpenFile(IMainForm MainForm, ActiveCodeFile code);
        void UpdateGUI(int col, int line);
        void ClearOutputStream();
        void ClearOpenFiles();
        bool HasFileOpen { get; }
        ActiveCodeFile GetCurrentActiveFile();
        void SetSaveInformationForActiveFile(string location);
    }
}
