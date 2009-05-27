using System.IO;
using Pyjama;

namespace Pyjama
{
    public interface IMainForm
    {
        CodeBlock GetCodeBlock();
        void OpenFile(IMainForm MainForm, ActiveCodeFile code);
        void UpdateGUI(int col, int line);
        bool HasFileOpen { get; }
        ActiveCodeFile GetCurrentActiveFile();
        void SetSaveInformationForActiveFile(string location);
        void Execute(string code);
        void SelectCommandShell();
        void ExecuteFile(string filename);
    }
}
