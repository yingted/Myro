using System.Drawing;
using System.IO;
using System.Windows.Forms;
using IronEditor.Engine;

namespace IronEditor.UI.WinForms.Controls
{
    public partial class FileManager : UserControl
    {
        private Font fontToUse;

        public FileManager()
        {
            InitializeComponent();
        }

        public bool HasFileOpen
        {
            get { return GetCurrentTab() != null; }
        }

        public Font FontToUse
        {
            get { return fontToUse; }
            set 
            { 
                fontToUse = value;
                UpdateActiveTabsFont(); 
            }
        }

        private void UpdateActiveTabsFont()
        {
            foreach (IDETab tab in tabControl.TabPages)
            {
                tab.Input.Font = FontToUse;
            }
        }


        public void OpenFile(ActiveCodeFile file)
        {
            IDETab tab = new IDETab(file);
            if (FontToUse != null)
                tab.Input.Font = FontToUse;
            tab.SetInitialText(File.ReadAllText(file.Location));
            tabControl.TabPages.Add(tab);
            tabControl.SelectedTab = tab;
        }

        public ActiveCodeFile GetCurrentFile()
        {
            if (HasFileOpen)
                return GetCurrentTab().ActiveFile;
            else
                return null;
        }

        public CodeBlock GetCode()
        {
            CodeBlock block = new CodeBlock();
            block.Code = GetCurrentInput();

            return block;
        }

        public IDEInput GetCurrentInput()
        {
            if (HasFileOpen)
                return GetCurrentTab().Input;
            else
                return null;
        }

        private IDETab GetCurrentTab()
        {
            if (tabControl.TabCount > 0) 
                return tabControl.SelectedTab as IDETab;
            
            return null;
        }

        public void Clear()
        {
            tabControl.TabPages.Clear();
        }

        public void SetSaveInformationForActiveFile(string location)
        {
            ActiveCodeFile code = GetCurrentFile();
            code.Location = location;
            code.Unsaved = false;
            code.Untitled = false;
            IDETab tab = GetCurrentTab();
            tab.UpdateFileName(code);
            tab.UpdateSaveStatus();
        }
    }
}
