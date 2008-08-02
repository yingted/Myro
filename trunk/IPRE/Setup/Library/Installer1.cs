using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration.Install;
using System.Linq;
using System.IO;
using System.Security.AccessControl;
using System.Windows.Forms;

namespace Library
{
    [RunInstaller(true)]
    public partial class Installer1 : Installer
    {
        public Installer1()
        {
            InitializeComponent();
        }

        public override void Install(IDictionary stateSaver)
        {
            base.Install(stateSaver);

            var installdir = GetParameter("targetdir");

            string msg = "";
            foreach (var k in Context.Parameters.Keys)
                msg += (k + "\n");
            msg += "VALUES:\n";
            foreach (var v in Context.Parameters.Values)
                msg += (v + "\n");
            //MessageBox.Show(msg);

            List<string> writeableDirs = new List<string>();

            string configDir = Path.Combine(Path.Combine(installdir, "Myro"), "config");
            string storeDir = Path.Combine(Path.Combine(installdir, "Myro"), "store");

            writeableDirs.Add(configDir);
            writeableDirs.Add(storeDir);
            writeableDirs.AddRange(Directory.GetDirectories(configDir, "*", SearchOption.AllDirectories));
            writeableDirs.AddRange(Directory.GetDirectories(storeDir, "*", SearchOption.AllDirectories));

            string dirs = "";
            foreach (var d in writeableDirs)
            {
                dirs += (d + "\n");
                if (GrantModifyAccessToFolder("Everyone", d) != true)
                    throw new Exception("Couldn't make " + d + " writeable");
            }

            //MessageBox.Show("Made writeable:\n" + dirs);

        }

        private string GetParameter(string parameterKey)
        {
            if (Context.Parameters[parameterKey] == null)
            { return String.Empty; }
            return Context.Parameters[parameterKey].Trim();
        }

        public static bool GrantModifyAccessToFolder(string windowsAccountUserName,
                                             string folderName)
        {
            DirectoryInfo directory = null;
            DirectorySecurity directorySecurity = null;
            FileSystemAccessRule rule = null;

            try
            {

                if (windowsAccountUserName.Length < 1) { return false; }
                if (folderName.Length < 1) { return false; }
                if (!Directory.Exists(folderName)) { return false; }

                directory = new DirectoryInfo(folderName);

                directorySecurity = directory.GetAccessControl();

                rule = new FileSystemAccessRule(windowsAccountUserName,
                                                FileSystemRights.Modify,
                                                InheritanceFlags.None |
                                                InheritanceFlags.ContainerInherit |
                                                InheritanceFlags.ObjectInherit,
                                                PropagationFlags.None,
                                                AccessControlType.Allow);

                directorySecurity.SetAccessRule(rule);

                directory.SetAccessControl(directorySecurity);

                return true;

            }
            catch (Exception) { throw; }
        }
    }
}
