using System.Drawing;
using System.IO;
using System.Windows.Forms;

namespace Pyjama
{
    public partial class DocumentManager : UserControl
    {
        private Font fontToUse;

        public DocumentManager()
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
            foreach (DocumentPage tab in tabControl.TabPages)
            {
                tab.Input.Font = FontToUse;
            }
        }

        public void OpenFile(IMainForm main_form)
        {
            DocumentPage tab = new DocumentPage(main_form);
            if (FontToUse != null)
                tab.Input.Font = FontToUse;
            //tab.SetInitialText();
            tabControl.TabPages.Add(tab);
            tabControl.SelectedTab = tab;
        }

        public void OpenFile(IMainForm main_form, ActiveCodeFile file)
        {
            DocumentPage tab = new DocumentPage(main_form, file);
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

        public DocumentInput GetCurrentInput()
        {
            if (HasFileOpen)
                return GetCurrentTab().Input;
            else
                return null;
        }

        public DocumentPage GetCurrentTab()
        {
            if (tabControl.TabCount > 0)
                return tabControl.SelectedTab as DocumentPage;

            return null;
        }

        public Control GetCurrentTabTextBox()
        {
            if (tabControl.TabCount > 0)
                return ((DocumentPage)tabControl.SelectedTab).textBox as Control;

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
            code.SetFileName(location);
            code.Unsaved = false;
            code.Untitled = false;
            DocumentPage tab = GetCurrentTab();
            tab.UpdateFileName(code);
            tab.UpdateSaveStatus();
        }
    }
}
