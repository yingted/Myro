using System;
using System.Text;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using Pyjama.Dialogs;

using IronPython.Hosting;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;

namespace Pyjama
{
    public class MyMemoryStream : MemoryStream
    {
        private IMainForm mainform;

        public MyMemoryStream(IMainForm form)
        {
            mainform = form;
        }

        public override void Write(Byte [] buffer, int offset, int count) {
            mainform.PrintConsoleMessage(Encoding.UTF8.GetString(buffer, offset, count));
        }
    }

    public class PyjamaFormController
    {
        public IMainForm pyjamaForm { get; set; }
        List<LanguageSettings> languages;
        private ScriptEngine engine;
        ScriptRuntime env;
        private ScriptScope scope;
        ScriptRuntimeSetup setup;
        MyMemoryStream ms;

        public PyjamaFormController(IMainForm mainForm)
        {
            pyjamaForm = mainForm;
            // Engine stuff:
            setup = new ScriptRuntimeSetup();
            setup.LanguageSetups.Add(IronPython.Hosting.Python.CreateLanguageSetup(null));
            //setup.LanguageSetups.Add(new LanguageSetup(assembly_qualified_name, displayName, languageNames, fileExtensions));
            //env = ScriptRuntime.CreateFromConfiguration();
            env = new ScriptRuntime(setup);
            engine = env.GetEngine("py"); // env.GetEngine("rb");
            scope = env.CreateScope();
            ms = new MyMemoryStream(pyjamaForm);
            env.IO.SetOutput(ms, new UTF8Encoding(false));

            LoadSettings();
            // load one from command line
            // or, open a blank one
            NewFile();
            pyjamaForm.PrintConsoleMessage("Pyjama Python, Version 1.0.0\r\n>>> ");

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
            MessageBox.Show("See http://PyjamaProject.org", "Help", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        internal void Execute()
        {
            // Get text to eval:
            System.String code = pyjamaForm.GetCodeBlock().GetCodeToExecute();
            code = code.Trim();
            pyjamaForm.PrintLineConsoleMessage(code);
            try
            {
                ScriptSource source = engine.CreateScriptSourceFromString(code, SourceCodeKind.InteractiveCode);
                object result = source.Execute(scope);
            }
            catch (System.Exception e1)
            {
                try
                {
                    ScriptSource source = engine.CreateScriptSourceFromString(code, SourceCodeKind.Statements);
                    pyjamaForm.PrintLineConsoleMessage("Evaluating...");
                    object result = source.Execute(scope);
                }
                catch (System.Exception e)
                {
                    pyjamaForm.PrintLineConsoleMessage("Exception: " + e.Message);
                }
            }
            pyjamaForm.PrintPrompt();
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
            pyjamaForm.OpenFile(pyjamaForm, file);
        }

        public void OpenFile()
        {
            OpenFileDialog open = new OpenFileDialog();
            open.Filter = "All Files (*.*)|*.*";

            if (open.ShowDialog() == DialogResult.OK)
            {
                ActiveCodeFile file = new ActiveCodeFile(open.FileName);
                file.Unsaved = false;
                OpenFile(file);
            }
        }

        public void OpenFile(ActiveCodeFile file)
        {
            if (File.Exists(file.Location))
                pyjamaForm.OpenFile(pyjamaForm, file);
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
            code.FileName = "Untitled";
            code.Unsaved = true;
            return code;
        }

        public void Save()
        {
            //System.Console.WriteLine("controller saving...");
            ActiveCodeFile activeFile = pyjamaForm.GetCurrentActiveFile();
            if (pyjamaForm.HasFileOpen && activeFile != null)
            {
                if (activeFile.Unsaved)
                {
                    string location = activeFile.Location;

                    if (string.IsNullOrEmpty(location) || activeFile.Untitled)
                        location = GetSaveLocation();
                    if (location != string.Empty)
                    {
                        WriteCodeToExecuteToFile(location);
                        //System.Console.WriteLine("saving to " + location);
                        pyjamaForm.SetSaveInformationForActiveFile(location);
                    }
                }
            }
        }

        private void WriteCodeToExecuteToFile(string file)
        {
            if (string.IsNullOrEmpty(file))
                return; // Do nothing then :P

            StreamWriter sw = new StreamWriter(file, false);

            try
            {
                sw.Write(pyjamaForm.GetCodeBlock().Code.Text);
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
                pyjamaForm.SetSaveInformationForActiveFile(location);
            }
        }
    }
}
