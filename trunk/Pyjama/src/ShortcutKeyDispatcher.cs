using System.Windows.Forms;

namespace Pyjama
{
    internal class ShortcutKeyDispatcher
    {
        private PyjamaFormController _controller;
        public ShortcutKeyDispatcher(PyjamaFormController controller)
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
                    _controller.ExecuteInThread();
                    break;
            }
        }
    }
}
