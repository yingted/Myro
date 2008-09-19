using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using IronEditor.Engine;
using IronEditor.UI.WinForms.Dialogs;

namespace IronEditor.UI.WinForms
{
    public class MainFormController
    {
        public IMainForm MainForm { get; set; }
        List<LanguageSettings> languages;
        private EngineCache EngineCache;

        public MainFormController(IMainForm mainForm)
        {
            MainForm = mainForm;
            EngineCache = new EngineCache();

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
            IEngine engine = GetEngineFromCache();

            engine.ExecuteStatement(MainForm.GetCodeBlock().GetCodeToExecute());
        }

        private IEngine GetEngineFromCache()
        {
            string languageExtension = MainForm.GetCurrentActiveFile().FileExtension;
            LanguageSettings setting = FindLanguageByExtension(languageExtension);

            return EngineCache.GetEngine(setting, MainForm.GetOutputStream());
        }

        public List<LanguageSettings> GetLanguages()
        {
            return languages;
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
            return languages.Find(l => l.Language == langauge).FileExtensions.Split(';');
        }

        public void Exit()
        {
            Application.Exit();
        }

        public void NewProject(MainFormController _controller)
        {
            NewProjectDialog newProjectDialog = new NewProjectDialog(_controller);
            if(newProjectDialog.ShowDialog() == DialogResult.OK)
            {
                if (newProjectDialog.ProjectType == "Project")
                {
                    Clear();
                    EngineCache.AppendPathToEngines(newProjectDialog.ProjectPath);
                    MainForm.OpenProject(newProjectDialog.ProjectPath);
                }
                else
                    OpenFile(CreateDefaultActiveFile(newProjectDialog.SelectedLanguage));
            }
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

                    WriteCodeToExecuteToFile(location);
                    MainForm.SetSaveInformationForActiveFile(location);
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
            saveFileDialog.Filter = GetEngineFromCache().GetSaveFilter();
            if (saveFileDialog.ShowDialog() == DialogResult.OK)
                return saveFileDialog.FileName;

            return string.Empty;
        }

        public void SaveAs()
        {
            string location = GetSaveLocation();
            WriteCodeToExecuteToFile(location);
            MainForm.SetSaveInformationForActiveFile(location);
        }

        public void OpenProject()
        {
            FolderBrowserDialog folderBrowserDialog = new FolderBrowserDialog();
            folderBrowserDialog.SelectedPath = Directory.GetCurrentDirectory();
            folderBrowserDialog.ShowNewFolderButton = false;
            folderBrowserDialog.Description = "Select the folder to load";
            if(folderBrowserDialog.ShowDialog() == DialogResult.OK)
            {
                Clear();
                MainForm.OpenProject(folderBrowserDialog.SelectedPath);
                EngineCache.AppendPathToEngines(folderBrowserDialog.SelectedPath);
            }
        }

        private void Clear()
        {
            MainForm.ClearOutputStream();
            MainForm.ClearProjectList();
            MainForm.ClearOpenFiles();
        }

        public void LaunchConsole()
        {
            if (MainForm.HasFileOpen)
            {
                string languageExtension = MainForm.GetCurrentActiveFile().FileExtension;
                LanguageSettings setting = FindLanguageByExtension(languageExtension);

                IEngine engine = EngineCache.GetEngine(setting, MainForm.GetOutputStream());
                engine.LaunchConsole();
            }
        }

        public bool CanLaunchConsole()
        {
            if (MainForm.HasFileOpen)
            {
                string languageExtension = MainForm.GetCurrentActiveFile().FileExtension;
                LanguageSettings setting = FindLanguageByExtension(languageExtension);

                IEngine engine = EngineCache.GetEngine(setting, MainForm.GetOutputStream());
                return engine.CanExecuteConsole;
            }

            return false;
        }
    }
}
