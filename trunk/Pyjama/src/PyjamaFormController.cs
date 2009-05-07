using System;
using System.Text;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using Pyjama.Dialogs;

namespace Pyjama
{
    public class PyjamaFormController
    {
        public IMainForm pyjamaForm { get; set; }
        List<LanguageSettings> languages;

        public PyjamaFormController(IMainForm mainForm)
        {
            pyjamaForm = mainForm;
            NewFile();
        }

        private void LoadSettings()
        {
            LanguageSettingsLoader loader = new LanguageSettingsLoader();

            if (Directory.Exists(ApplicationOptions.SettingsDirectory))
                languages = loader.LoadSettings(ApplicationOptions.SettingsDirectory);
            else
            {
                languages = new List<LanguageSettings>(); //HACK: Not sure what to do about this.
                //MessageBox.Show("Config directory not found. No language information loaded.",
                //                "Config directory not found", MessageBoxButtons.OK, MessageBoxIcon.Error);
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

        internal void ExecuteInThread()
        {
            // Get text to eval:
            System.String code = pyjamaForm.GetCodeBlock().GetCodeToExecute();
            code = code.Trim();
            pyjamaForm.Execute(code);
        }

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
