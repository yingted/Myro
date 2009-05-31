using System;
using System.Drawing; 
using System.Drawing.Printing;
using System.Collections; 
using System.Windows.Forms;

namespace Pyjama
{
    public interface IPrintable
    {
        void Print(PrintElement element);
    }
    public interface IPrintPrimitive
    {
        float CalculateHeight(PrintEngine engine, Graphics graphics);
        void Draw(PrintEngine engine, float yPos, Graphics graphics, Rectangle elementBounds);
    }
    public class PrintPrimitiveRule : IPrintPrimitive
    {
        public float CalculateHeight(PrintEngine engine, Graphics graphics)
        {
            return 5;
        }
        public void Draw(PrintEngine engine, float yPos, Graphics graphics, Rectangle elementBounds)
        {
            Pen pen = new Pen(engine.PrintBrush, 1);
            graphics.DrawLine(pen, elementBounds.Left, yPos + 2,
                              elementBounds.Right, yPos + 2);
        }
    }
    public class PrintPrimitiveText : IPrintPrimitive
    {
        public String Text;
        public PrintPrimitiveText(String buf)
        {
            Text = buf;
        }
        public float CalculateHeight(PrintEngine engine, Graphics graphics)
        {
            return engine.PrintFont.GetHeight(graphics);
        }
        public void Draw(PrintEngine engine, float yPos, Graphics graphics, Rectangle elementBounds)
        {
            graphics.DrawString(engine.ReplaceTokens(Text), engine.PrintFont,
                                engine.PrintBrush, elementBounds.Left, yPos, new StringFormat());
        }
    }
    public class PrintElement
    {
        private ArrayList _printPrimitives = new ArrayList();
        private IPrintable _printObject;

        public PrintElement(IPrintable printObject)
        {
            _printObject = printObject;
        }
        public void AddText(String buf)
        {
            AddPrimitive(new PrintPrimitiveText(buf));
        }
        public void AddPrimitive(IPrintPrimitive primitive)
        {
            _printPrimitives.Add(primitive);
        }
        public void Print()
        {
            _printObject.Print(this);
        }
        public void AddData(String dataName, String dataValue)
        {
            AddText(dataName + ": " + dataValue);
        }
        public void AddHorizontalRule()
        {
            AddPrimitive(new PrintPrimitiveRule());
        }
        public void AddBlankLine()
        {
            AddText("");
        }
        public void AddSection(String buf)
        {
            AddText(buf);
            AddHorizontalRule();
        }
        public float CalculateHeight(PrintEngine engine, Graphics graphics)
        {
            float height = 0;
            foreach (IPrintPrimitive primitive in _printPrimitives)
            {
                height += primitive.CalculateHeight(engine, graphics);
            }
            return height;
        }
        public void Draw(PrintEngine engine, float yPos, Graphics graphics, Rectangle pageBounds)
        {
            float height = CalculateHeight(engine, graphics);
            Rectangle elementBounds = new Rectangle(pageBounds.Left, (int)yPos, pageBounds.Right - pageBounds.Left, (int)height);
            foreach (IPrintPrimitive primitive in _printPrimitives)
            {
                primitive.Draw(engine, yPos, graphics, elementBounds);
                yPos += primitive.CalculateHeight(engine, graphics);
            }
        }
    }
    public class PrintEngine : PrintDocument
    {
        private ArrayList _printObjects = null;
        public Font PrintFont = new Font("Arial", 10);
        public Brush PrintBrush = Brushes.Black;
        public PrintElement Header;
        public PrintElement Footer;
        private ArrayList _printElements;
        private int _printIndex = 0;
        private int _pageNum = 0;
        PyjamaFormController _form;

        public PrintEngine(PyjamaFormController form)
        {
            _form = form;
            InitializeControl("Untitled");
        }

        private void InitializeControl(string filename) {
            _printObjects = new ArrayList();
            Header = new PrintElement(null);
            Header.AddText("File: " + filename);
            Header.AddText("Page: [pagenum]");
            Header.AddHorizontalRule();
            Header.AddBlankLine();
            Footer = new PrintElement(null);
            Footer.AddBlankLine();
            Footer.AddHorizontalRule();
            DateTime dt = DateTime.Now;
            Footer.AddText("Printed on " + String.Format("{0:f}", dt));
        }

        public void AddPrintObject(IPrintable printObject)
        {
            _printObjects.Add(printObject);
        }
        public String ReplaceTokens(String buf)
        {
            buf = buf.Replace("[pagenum]", _pageNum.ToString());
            return buf;
        }
        protected override void OnBeginPrint(PrintEventArgs e)
        {
            _printElements = new ArrayList();
            _pageNum = 0;
            _printIndex = 0;
            foreach (IPrintable printObject in _printObjects)
            {
                PrintElement element = new PrintElement(printObject);
                _printElements.Add(element);
                element.Print();
            }
        }
        protected override void OnPrintPage(PrintPageEventArgs e)
        {
            _pageNum++;
            float headerHeight = Header.CalculateHeight(this, e.Graphics);
            Header.Draw(this, e.MarginBounds.Top, e.Graphics, e.MarginBounds);
            float footerHeight = Footer.CalculateHeight(this, e.Graphics);
            Footer.Draw(this, e.MarginBounds.Bottom - footerHeight, e.Graphics, e.MarginBounds);
            Rectangle pageBounds = new Rectangle(e.MarginBounds.Left,
                  (int)(e.MarginBounds.Top + headerHeight), e.MarginBounds.Width,
                  (int)(e.MarginBounds.Height - footerHeight - headerHeight));
            float yPos = pageBounds.Top;
            bool morePages = false;
            int elementsOnPage = 0;
            while (_printIndex < _printElements.Count)
            {
                PrintElement element = (PrintElement)_printElements[_printIndex];
                float height = element.CalculateHeight(this, e.Graphics);
                if (yPos + height > pageBounds.Bottom)
                {
                    if (elementsOnPage != 0)
                    {
                        morePages = true;
                        break;
                    }
                }
                element.Draw(this, yPos, e.Graphics, pageBounds);
                yPos += height;
                _printIndex++;
                elementsOnPage++;
            }
            e.HasMorePages = morePages;
        }

        public void ShowPreview()
        {
            InitializeControl(_form.pyjamaForm.GetCurrentActiveFile().Location);
            RenderDocument();
            PrintPreviewDialog dialog = new PrintPreviewDialog();
            dialog.Document = this;
            // FIXME Windows7
            //dialog.UseEXDialog = true; 
            dialog.ShowDialog();
        }
        public void ShowPrintDialog()
        {
            InitializeControl(_form.pyjamaForm.GetCurrentActiveFile().Location);
            RenderDocument();
            PrintDialog dialog = new PrintDialog();
            dialog.PrinterSettings = PrinterSettings;
            dialog.Document = this;
            if (dialog.ShowDialog() == DialogResult.OK)
            {
                PrinterSettings = dialog.PrinterSettings;
                Print();
            }
        }
        public void ShowPageSettings()
        {
            PageSetupDialog setup = new PageSetupDialog();
            PageSettings settings = DefaultPageSettings;
            setup.PageSettings = settings;
            if (setup.ShowDialog() == DialogResult.OK)
                DefaultPageSettings = setup.PageSettings;
        }

        public void RenderDocument()
        {
            string text = _form.pyjamaForm.GetCodeBlock().Code.Text;
            foreach (string line in text.Split('\n'))
            {
                AddPrintObject(new PrintableString(line));
            }

        }

    }

    public class PrintableString : IPrintable
    {
        string Text;
        public PrintableString(string s)
        {
            Text = s;
        }
        public void Print(PrintElement element) {
            element.AddText(Text);
        }
    }
}