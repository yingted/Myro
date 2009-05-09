using System;
using System.Drawing;

namespace IronEditor.UI.WinForms
{
    [Serializable]
    public class UserSettings
    {
        public string FontName { get; set; }
        public float FontSize { get; set; }
        public Font UIFont
        {
            get
            {
                return new Font(FontName, FontSize);
            }
        }
        public Font ConsoleFont
        {
            get
            {
                return new Font(FontName, FontSize);
            }
        }
    }
}
