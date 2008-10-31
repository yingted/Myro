using System;
using System.Text;
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
        ScriptRuntime env;
        private ScriptScope scope;
        ScriptRuntimeSetup setup;
        MemoryStream ms;
        int current_position = -1;

        public MainFormController(IMainForm mainForm)
        {
            MainForm = mainForm;
            // Engine stuff:
            setup = new ScriptRuntimeSetup();
            setup.LanguageSetups.Add(IronPython.Hosting.Python.CreateLanguageSetup(null));
            //setup.LanguageSetups.Add(new LanguageSetup(assembly_qualified_name, displayName, languageNames, fileExtensions));
            //env = ScriptRuntime.CreateFromConfiguration();
            env = new ScriptRuntime(setup);
            ms = new MemoryStream();
            env.IO.SetOutput(ms, new StreamWriter(ms)); // RedirectToConsole();
            engine = env.GetEngine("py"); // env.GetEngine("rb");
            scope = env.CreateScope();
          
            LoadSettings();
            // load one from command line
            // or, open a blank one
            NewFile();
            MainForm.PrintConsoleMessage("Pyjama Python, Version 1.0.0\r\n>>> ");
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
            MessageBox.Show("See http://pyjamaproject.org", "Help", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        private static string ReadFromStream(int current_position, MemoryStream ms)
        {
            int length = ((int)ms.Length);
            System.Console.WriteLine(String.Format("current_position={0}, ms.Length={1}, length={2}", current_position, ms.Length, length));
            Byte[] bytes = new Byte[length];
            ms.Seek(0, SeekOrigin.Begin);
            ms.Read(bytes, 0, length);
            System.Console.WriteLine(String.Format("current_position={0}, length={1}, bytes={2}", current_position, length, 
                Encoding.GetEncoding("utf-8").GetString(bytes)));
            return Encoding.GetEncoding("utf-8").GetString(bytes);
        }

        internal void Execute()
        {
            System.String code = MainForm.GetCodeBlock().GetCodeToExecute();
            ms.Flush();
            try
            {
                ScriptSource source = engine.CreateScriptSourceFromString(code, SourceCodeKind.InteractiveCode);
                object result = source.Execute(scope);
                string str = ReadFromStream(current_position, ms);
                current_position = ((int)ms.Length) - 1;
                MainForm.PrintConsoleMessage("Evaluate: " + code);
                MainForm.PrintConsoleMessage(str);
            } catch (System.Exception e1) {
                try
                {
                    ScriptSource source = engine.CreateScriptSourceFromString(code, SourceCodeKind.Statements);
                    object result = source.Execute(scope);
                    string str = ReadFromStream(current_position, ms);
                    current_position = ((int)ms.Length) - 1;
                    MainForm.PrintConsoleMessage(str);
                }
                catch (System.Exception e)
                {
                    MainForm.PrintConsoleMessage(e.Message);
                }
            }
            MainForm.PrintConsoleMessage(">>> ");
        }

//  Message="unexpected token 'print'"
//  Source="Microsoft.Scripting"
//  Column=1
//  ErrorCode=16
//  Line=1
//  SourceCode="print \"hello\"\r\n"
//  StackTrace:
       
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
