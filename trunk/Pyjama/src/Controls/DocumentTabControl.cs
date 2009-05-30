using System;
using System.Windows.Forms;
using System.Drawing;

namespace Pyjama
{
    public delegate bool PreRemoveTab(int indx);

    class DocumentTabControl : System.Windows.Forms.TabControl
    {

        public PreRemoveTab PreRemoveTabPage;

         public DocumentTabControl()
            : base()
        {
            PreRemoveTabPage = null;
            this.DrawMode = TabDrawMode.OwnerDrawFixed;
        }

        protected override void OnDrawItem(DrawItemEventArgs e)
        {
            Rectangle r = e.Bounds;
            r = GetTabRect(e.Index);
            r.Offset(2, 2);
            r.Width = 5;
            r.Height = 5;
            Brush b = new SolidBrush(Color.Black);
            Pen p = new Pen(b);
            string title = this.TabPages[e.Index].Text;
            Font f = this.Font;
            e.Graphics.DrawString(title, f, b, new PointF(r.X + 5, r.Y));
            e.Graphics.DrawLine(p, r.X, r.Y, r.X + r.Width, r.Y + r.Height);
            e.Graphics.DrawLine(p, r.X + r.Width, r.Y, r.X, r.Y + r.Height);
            //e.Graphics.DrawLine(p, r.X, r.Y, r.X + r.Width, r.Y);
            //e.Graphics.DrawLine(p, r.X, r.Y, r.X, r.Y + r.Height);
        }
        protected override void OnMouseClick(MouseEventArgs e)
        {
            Point p = e.Location;
            for (int i = 0; i < TabCount; i++)
            {
                Rectangle r = GetTabRect(i);
                r.Offset(2, 2);
                r.Width = 5;
                r.Height = 5;
                if (r.Contains(p))
                {
                    CloseTab(i);
                }
            }
        }

        private void CloseTab(int i)
        {
            if (PreRemoveTabPage != null)
            {
                bool closeIt = PreRemoveTabPage(i);
                if (!closeIt)
                    return;
            }
            TabPages.Remove(TabPages[i]);
        }

        private void InitializeComponent()
        {
            this.SuspendLayout();
            this.ResumeLayout(false);

        }
    }
}