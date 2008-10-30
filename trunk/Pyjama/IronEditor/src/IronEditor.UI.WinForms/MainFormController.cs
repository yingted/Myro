using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using IronEditor.UI.WinForms.Dialogs;

using IronPython.Hosting;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;

namespace IronEditor.UI.WinForms
{
    public class MainFormController
    {
        public IMainForm MainForm { get; set; }
        List<LanguageSettings> languages;
        private ScriptEngine engine;
        private ScriptScope scope;

        public MainFormController(IMainForm mainForm)
        {
            MainForm = mainForm;
            // Engine stuff:
            engine = IronPython.Hosting.Python.CreateEngine();
            scope = engine.Runtime.CreateScope();
          
            LoadSettings();
        }

        private void LoadSettings()
        {
            LanguageSettingsLoader loader = new LanguageSettingsLoader();

            if (Directory.Exists(ApplicationOptions.SettingsDirectory))
                languages = loader.LoadSettings(ApplicationOptions.SettingsDirectory);
            else
            {
                languages = new List<LanguageSettings>(); //HACK: Not sure what to do about this.
                MessageBox.Show("Config directory not found. No language information loaded.",
                                "Config directory not found", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        public void DisplayAboutDialog()
        {
            AboutDialog about = new AboutDialog();
            about.ShowDialog();
        }

        public void LaunchHelp()
        {
            MessageBox.Show("Coming soon! Maybe... \r\nIn the meantime, visit Blog.BenHall.me.uk", "Help", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        internal void Execute()
        {
            //IEngine engine = GetEngineFromCache();

            //engine.ExecuteStatement(MainForm.GetCodeBlock().GetCodeToExecute());

            System.Console.WriteLine(MainForm.GetCodeBlock().GetCodeToExecute());

            try
            {
                System.String code = MainForm.GetCodeBlock().GetCodeToExecute();
                ScriptSource source = engine.CreateScriptSourceFromString(code, SourceCodeKind.InteractiveCode);
                object result = source.Execute(scope);
                System.Console.WriteLine(result);
            } 
            catch (System.Exception e)
            {
                System.Console.WriteLine("Exception: {0}", e.Message);
            }

//  Message="unexpected token 'print'"
//  Source="Microsoft.Scripting"
//  Column=1
//  ErrorCode=16
//  Line=1
//  SourceCode="print \"hello\"\r\n"
//  StackTrace:
       
        }

        /*
        private IEngine GetEngineFromCache()
        {
            string languageExtension = MainForm.GetCurrentActiveFile().FileExtension;
            LanguageSettings setting = FindLanguageByExtension(languageExtension);

            return EngineCache.GetEngine(setting, MainForm.GetOutputStream());
        }
        */

        public List<LanguageSettings> GetLanguages()
        {
            return languages;
        }

        public void NewFile()
        {
            ActiveCodeFile file = CreateDefaultActiveFile("Python");
            MainForm.OpenFile(file);
        }

        public void OpenFile()
        {
            OpenFileDialog open = new OpenFileDialog();
            open.Filter = "All Files (*.*)|*.*";

            if(open.ShowDialog() == DialogResult.OK)
            {
                ActiveCodeFile file = new ActiveCodeFile(open.FileName);
                file.Unsaved = false;
                OpenFile(file);
            }
        }

        public void OpenFile(ActiveCodeFile file)
        {
            if (File.Exists(file.Location))
                MainForm.OpenFile(file);
            else
                throw new FileNotFoundException("File not found", file.Location);
        }

        private LanguageSettings FindLanguageByExtension(string ext)
        {
            return languages.Find(l => l.FileExtensions.Contains(ext));
        }

        private string[] FindExtensionByLanguage(string langauge)
        {
            if (languages != null)
                return languages.Find(l => l.Language == langauge).FileExtensions.Split(';');
            else
                return new string[0];
        }

        public void Exit()
        {
            Application.Exit();
        }

        private ActiveCodeFile CreateDefaultActiveFile(string language)
        {
            string[] fileExt = FindExtensionByLanguage(language);
            string usedFileExt = fileExt.Length > 0 ? fileExt[0] : ApplicationOptions.DefaultExtension;

            string file = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + usedFileExt;
            string path = Path.Combine(Path.GetTempPath(), file);
            using (StreamWriter sw = new StreamWriter(path))
                sw.Close();

            ActiveCodeFile code = new ActiveCodeFile();
            code.Location = path;
            code.Untitled = true;
            code.FileName = "New File";
            code.Unsaved = true;
            return code;
        }

        public void Save()
        {
            ActiveCodeFile activeFile = MainForm.GetCurrentActiveFile();
            if (MainForm.HasFileOpen && activeFile != null)
            {
                if (activeFile.Unsaved)
                {
                    string location = activeFile.Location;

                    if (string.IsNullOrEmpty(location) || activeFile.Untitled)
                        location = GetSaveLocation();
                    if (location != string.Empty)
                    {
                        WriteCodeToExecuteToFile(location);
                        MainForm.SetSaveInformationForActiveFile(location);
                    }
                }
            }
        }

        private void WriteCodeToExecuteToFile(string file)
        {
            if (string.IsNullOrEmpty(file))
                return; // Do nothing then :P

            StreamWriter sw = new StreamWriter(file,false);

            try
            {
                sw.Write(MainForm.GetCodeBlock().Code.Text);
                sw.Flush();
            }
            finally
            {
                sw.Close();                
            }
        }

        private string GetSaveLocation()
        {
            SaveFileDialog saveFileDialog = new SaveFileDialog();
            //saveFileDialog.Filter = GetEngineFromCache().GetSaveFilter();
            if (saveFileDialog.ShowDialog() == DialogResult.OK)
                return saveFileDialog.FileName;
            return string.Empty;
        }

        public void SaveAs()
        {
            string location = GetSaveLocation();
            if (location != string.Empty)
            {
                WriteCodeToExecuteToFile(location);
                MainForm.SetSaveInformationForActiveFile(location);
            }
        }

        private void Clear()
        {
            MainForm.ClearOutputStream();
            MainForm.ClearOpenFiles();
        }

        public void LaunchConsole()
        {
            if (MainForm.HasFileOpen)
            {
                string languageExtension = MainForm.GetCurrentActiveFile().FileExtension;
                LanguageSettings setting = FindLanguageByExtension(languageExtension);

                //IEngine engine = EngineCache.GetEngine(setting, MainForm.GetOutputStream());
                //engine.LaunchConsole();
            }
        }

        public bool CanLaunchConsole()
        {
            if (MainForm.HasFileOpen)
            {
                string languageExtension = MainForm.GetCurrentActiveFile().FileExtension;
                LanguageSettings setting = FindLanguageByExtension(languageExtension);

                //IEngine engine = EngineCache.GetEngine(setting, MainForm.GetOutputStream());
                //return engine.CanExecuteConsole;
                return false;
            }

            return false;
        }
    }
}
