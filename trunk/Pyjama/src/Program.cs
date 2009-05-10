using System;
using System.Windows.Forms;

namespace Pyjama
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main(string [] args)
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            ApplicationOptions.LoadOptions();
            Application.Run(new PyjamaForm(args));
        }
    }
}
