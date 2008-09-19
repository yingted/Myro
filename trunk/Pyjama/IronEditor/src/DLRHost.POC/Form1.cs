using System;
using System.Reflection;
using System.Windows.Forms;
using Microsoft.Scripting.Hosting;
using System.Linq;

namespace DLRHost
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

   
        private void Form1_Load(object sender, EventArgs e)
        {
            codeEditorControl1.Document.Text = "print 'Hello World'";

            ScriptRuntimeSetup setup = new ScriptRuntimeSetup();
            setup.LanguageProviders = new[]
                                          {
                                              new LanguageProviderSetup("IronPython.Runtime.PythonContext", "IronPython", ".py", "ironpython")
                                          };
            ScriptRuntime runtime = ScriptRuntime.Create(setup);
            ScriptEngine engine = runtime.GetEngine("IronPython");
            foreach (string s in engine.GetRegisteredExtensions())
            {
                System.Console.WriteLine(s);
            }
        }
    }


}
