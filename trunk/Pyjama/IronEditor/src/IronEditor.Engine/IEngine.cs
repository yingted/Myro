using System.IO;

namespace IronEditor.Engine
{
    public interface IEngine
    {
        void ExecuteStatement(string code);
        string GetSaveFilter();
        string LanguageName { get; }
        bool CanExecuteConsole { get; }
        void AddPath(string path);
        string[] ScriptSourceSearch();
        void LaunchConsole();
    }

}