using System;
using System.Windows.Forms;

namespace IronEditor.UI.WinForms
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            ApplicationOptions.LoadOptions();
            Application.Run(new mainForm());
        }
    }
}
