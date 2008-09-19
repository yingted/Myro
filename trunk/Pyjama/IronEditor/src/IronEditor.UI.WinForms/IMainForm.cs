using System.IO;
using IronEditor.Engine;

namespace IronEditor.UI.WinForms
{
    public interface IMainForm
    {
        TextWriter GetOutputStream();
        CodeBlock GetCodeBlock();
        void OpenFile(ActiveCodeFile code);
        void ClearOutputStream();
        void ClearProjectList();
        void ClearOpenFiles();
        void OpenProject(string folder);
        bool HasFileOpen { get; }
        ActiveCodeFile GetCurrentActiveFile();
        void SetSaveInformationForActiveFile(string location);
    }
}
