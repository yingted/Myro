using System.Reflection;

namespace IronEditor.Engine
{
    public static class ApplicationInformation
    {
        public static string Title()
        {
            return "Pyjama Editor";
        }
        public static string Version()
        {
            return Assembly.GetExecutingAssembly().GetName().Version.ToString();
        }
    }
}
