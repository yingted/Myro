using System.Windows.Forms;

namespace IronEditor.UI.WinForms
{
    internal class ShortcutKeyDispatcher
    {
        private MainFormController _controller;
        public ShortcutKeyDispatcher(MainFormController controller)
        {
            _controller = controller;
        }

        public void Dispatch(KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.F1:
                    _controller.LaunchHelp();
                    break;
                case Keys.F5:
                    _controller.Execute();
                    break;
                case Keys.F6:
                    _controller.LaunchConsole();
                    break;
            }
        }
    }
}
