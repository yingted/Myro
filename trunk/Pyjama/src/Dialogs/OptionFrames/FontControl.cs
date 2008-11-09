using System;
using System.Drawing;
using System.Drawing.Text;
using System.Windows.Forms;

namespace Pyjama.Dialogs.OptionFrames
{
    public partial class FontControl : UserControl
    {
        private UserSettings Settings;
        public FontControl(UserSettings settings)
        {
            InitializeComponent();
            Settings = settings;
        }

        private void LoadFontOptions()
        {
            if (Settings != null)
            {
                SetFont(Settings.FontName);
                SetSize(Settings.FontSize);
            }
        }

        private void FontControl_Load(object sender, EventArgs e)
        {
            for (float i = 8; i < 26; i = i + 2)
            {
                size.Items.Add(i);
                if (i > (Font.Size -1) && i < (Font.Size + 1))
                    size.SelectedItem = i;
            }


            InstalledFontCollection installedFontCollection = new InstalledFontCollection();
            foreach (FontFamily fontFamily in installedFontCollection.Families)
            {
                font.Items.Add(fontFamily.Name);

                if (fontFamily.Name == Font.FontFamily.Name)
                    font.SelectedItem = fontFamily.Name;
            }

            LoadFontOptions();
        }

        internal void SetSize(float fontSize)
        {
            for (int i = 0; i < size.Items.Count; i++)
            {
                if (size.Items[i].ToString() == fontSize.ToString())
                    size.SelectedIndex = i;
            }
        }

        internal void SetFont(string fontFamily)
        {
            for (int i = 0; i < font.Items.Count; i++)
            {
                if (font.Items[i].ToString() == fontFamily)
                    font.SelectedIndex = i;
            }
        }


        internal float GetSize()
        {
            return (float)size.SelectedItem;
        }

        internal string GetFont()
        {
            return font.SelectedItem.ToString();
        }
    }
}
