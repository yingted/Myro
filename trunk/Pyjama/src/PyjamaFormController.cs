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

        public PyjamaFormController(IMainForm mainForm)
        {
            pyjamaForm = mainForm;
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
            // F5 Method.
            // Get text to eval:
            // FIXME: check to see where focus is
            CodeBlock codeBlock = pyjamaForm.GetCodeBlock();
            if (codeBlock.IsSnippet())
            {
                string code = codeBlock.GetCodeToExecute();
                code = code.Trim();
                code = code.Replace("\n", "\r\n");
                pyjamaForm.Execute(code);
            }
            else
            {
                Save();
                ActiveCodeFile activeFile = pyjamaForm.GetCurrentActiveFile();
                if (!activeFile.Unsaved) {
                    string filename = pyjamaForm.GetCurrentActiveFile().Location;
                    pyjamaForm.ExecuteFile(filename); // FIXME: pass in language
                }
            }
        }

        public void SelectCommandShell()
        {
            pyjamaForm.SelectCommandShell();
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

        public void OpenFile(string filename)
        {
	        ActiveCodeFile file = new ActiveCodeFile(filename);
            file.Unsaved = false;
	        OpenFile(file);
        }

        public void OpenFile(ActiveCodeFile file)
        {
            if (File.Exists(file.Location))
                pyjamaForm.OpenFile(pyjamaForm, file);
            else
                throw new FileNotFoundException("File not found", file.Location);
        }

        public void Exit()
        {
            bool done = false;
            System.Console.Write("Pyjama is shutting down...");
            while (!done) {
                try
                {
                    Application.ExitThread(); // Was Exit(), but that caused errors with other windows
                    done = true;
                }
                catch
                {
                    System.Console.Write(".");
                }
            }
            System.Console.WriteLine(" done!");
        }

        private ActiveCodeFile CreateDefaultActiveFile(string language)
        {
	  // FIXME: connect to loaded languages in Shell
            string usedFileExt = ApplicationOptions.DefaultExtension;
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
