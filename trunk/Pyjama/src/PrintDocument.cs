using System;
using System.Drawing; 
using System.Drawing.Printing;
using System.Collections; 
using System.Windows.Forms;

namespace Pyjama
{
    public class Form1 : Form
    {
        private TrackBar trackCustomers;
        private Button cmdPrintPreview;
        private Button cmdPageSettings;
        private Button cmdPrint;
        // members...
        PrintEngine _engine = new PrintEngine();


        public Form1()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            // create a default print engine...
            CreateEngine(1);
        }
        // CreateEngine - create a print engine and populate it with customers...
        public void CreateEngine(int numCustomers)
        {
            // create a new engine...
            _engine = new PrintEngine();

            // loop through the customers...
            for (int n = 0; n < numCustomers; n++)
            {
                // create the customer...
                Customer theCustomer = new Customer();
                theCustomer.Id = n + 1;
                theCustomer.FirstName = "Darren";
                theCustomer.LastName = "Clarke";
                theCustomer.Company = "Madras inc.";
                theCustomer.Email = "darren@pretendcompany.com";
                theCustomer.Phone = "602 555 1234";

                // add the customer to the list...
                _engine.AddPrintObject(theCustomer);
            }
        }

        private void InitializeComponent()
        {
            this.trackCustomers = new System.Windows.Forms.TrackBar();
            this.cmdPrintPreview = new System.Windows.Forms.Button();
            this.cmdPageSettings = new System.Windows.Forms.Button();
            this.cmdPrint = new System.Windows.Forms.Button();
            ((System.ComponentModel.ISupportInitialize)(this.trackCustomers)).BeginInit();
            this.SuspendLayout();
            // 
            // trackCustomers
            // 
            this.trackCustomers.Location = new System.Drawing.Point(26, 42);
            this.trackCustomers.Maximum = 128;
            this.trackCustomers.Name = "trackCustomers";
            this.trackCustomers.Size = new System.Drawing.Size(237, 45);
            this.trackCustomers.TabIndex = 0;
            this.trackCustomers.Scroll += new System.EventHandler(this.trackCustomers_Scroll);
            // 
            // cmdPrintPreview
            // 
            this.cmdPrintPreview.Location = new System.Drawing.Point(69, 108);
            this.cmdPrintPreview.Name = "cmdPrintPreview";
            this.cmdPrintPreview.Size = new System.Drawing.Size(97, 39);
            this.cmdPrintPreview.TabIndex = 1;
            this.cmdPrintPreview.Text = "Print Preview";
            this.cmdPrintPreview.UseVisualStyleBackColor = true;
            this.cmdPrintPreview.Click += new System.EventHandler(this.cmdPrintPreview_Click);
            // 
            // cmdPageSettings
            // 
            this.cmdPageSettings.Location = new System.Drawing.Point(69, 165);
            this.cmdPageSettings.Name = "cmdPageSettings";
            this.cmdPageSettings.Size = new System.Drawing.Size(96, 35);
            this.cmdPageSettings.TabIndex = 2;
            this.cmdPageSettings.Text = "Page Settings";
            this.cmdPageSettings.UseVisualStyleBackColor = true;
            this.cmdPageSettings.Click += new System.EventHandler(this.cmdPageSettings_Click);
            // 
            // cmdPrint
            // 
            this.cmdPrint.Location = new System.Drawing.Point(70, 214);
            this.cmdPrint.Name = "cmdPrint";
            this.cmdPrint.Size = new System.Drawing.Size(94, 41);
            this.cmdPrint.TabIndex = 3;
            this.cmdPrint.Text = "Print";
            this.cmdPrint.UseVisualStyleBackColor = true;
            this.cmdPrint.Click += new System.EventHandler(this.cmdPrint_Click);
            // 
            // Form1
            // 
            this.ClientSize = new System.Drawing.Size(284, 264);
            this.Controls.Add(this.cmdPrint);
            this.Controls.Add(this.cmdPageSettings);
            this.Controls.Add(this.cmdPrintPreview);
            this.Controls.Add(this.trackCustomers);
            this.Name = "Form1";
            ((System.ComponentModel.ISupportInitialize)(this.trackCustomers)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        private void cmdPageSettings_Click(object sender, EventArgs e)
        {
            _engine.ShowPageSettings(); 
        }

        private void cmdPrintPreview_Click(object sender, EventArgs e)
        {
            _engine.ShowPreview();
        }

        private void cmdPrint_Click(object sender, EventArgs e)
        {
            _engine.ShowPrintDialog(); 
        }

        private void trackCustomers_Scroll(object sender, EventArgs e)
        {
            CreateEngine(this.trackCustomers.Value);
        }

    }
    public interface IPrintable
    {
        void Print(PrintElement element);
    }
    public interface IPrintPrimitive
    {
        // CalculateHeight - work out how tall the primitive is...
        float CalculateHeight(PrintEngine engine, Graphics graphics);

        // Print - tell the primitive to physically draw itself...
        void Draw(PrintEngine engine, float yPos, Graphics graphics, Rectangle elementBounds);
    }
    public class PrintPrimitiveRule : IPrintPrimitive
    {
        // CalculateHeight - work out how tall the primitive is...
        public float CalculateHeight(PrintEngine engine, Graphics graphics)
        {
            // we're always five units tall...
            return 5;
        }
        // Print - draw the rule...
        public void Draw(PrintEngine engine, float yPos, Graphics graphics, Rectangle elementBounds)
        {
            // draw a line...
            Pen pen = new Pen(engine.PrintBrush, 1);
            graphics.DrawLine(pen, elementBounds.Left, yPos + 2,
                          elementBounds.Right, yPos + 2);
        }
    }
    public class PrintPrimitiveText : IPrintPrimitive
    {
        // members...
        public String Text;
        public PrintPrimitiveText(String buf)
        {
            Text = buf;
        }
        // CalculateHeight - work out how tall the primitive is...
        public float CalculateHeight(PrintEngine engine, Graphics graphics)
        {
            // return the height...
            return engine.PrintFont.GetHeight(graphics);
        }
        public void Draw(PrintEngine engine, float yPos, Graphics graphics, Rectangle elementBounds)
        {
            // draw it...
            graphics.DrawString(engine.ReplaceTokens(Text), engine.PrintFont,
            engine.PrintBrush, elementBounds.Left, yPos, new StringFormat());
        }
    }
    public class PrintElement
    {
        // members...
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
            // add this data to the collection...
            AddText(dataName + ": " + dataValue);
        }
        public void AddHorizontalRule()
        {
            // add a rule object...
            AddPrimitive(new PrintPrimitiveRule());
        }
        public void AddBlankLine()
        {
            // add a blank line...
            AddText("");
        }
        public void AddHeader(String buf)
        {
            AddText(buf);
            AddHorizontalRule();
        }
        public float CalculateHeight(PrintEngine engine, Graphics graphics)
        {
            // loop through the print height...
            float height = 0;
            foreach (IPrintPrimitive primitive in _printPrimitives)
            {
                // get the height...
                height += primitive.CalculateHeight(engine, graphics);
            }

            // return the height...
            return height;
        }
        public void Draw(PrintEngine engine, float yPos, Graphics graphics, Rectangle pageBounds)
        {
            // where...
            float height = CalculateHeight(engine, graphics);
            Rectangle elementBounds = new Rectangle(pageBounds.Left, (int)yPos, pageBounds.Right - pageBounds.Left, (int)height);

            // now, tell the primitives to print themselves...
            foreach (IPrintPrimitive primitive in _printPrimitives)
            {
                // render it...
                primitive.Draw(engine, yPos, graphics, elementBounds);

                // move to the next line...
                yPos += primitive.CalculateHeight(engine, graphics);
            }
        }
    }
    public class PrintEngine : PrintDocument
    {
        private ArrayList _printObjects = new ArrayList();
        public Font PrintFont = new Font("Arial", 10);
        public Brush PrintBrush = Brushes.Black;
        public PrintElement Header;
        public PrintElement Footer;
        private ArrayList _printElements;
        private int _printIndex = 0;
        private int _pageNum = 0;

        public PrintEngine()
        {
            // create the header...
            Header = new PrintElement(null);
            Header.AddText("Report");
            Header.AddText("Page: [pagenum]");
            Header.AddHorizontalRule();
            Header.AddBlankLine();

            // create the footer...
            Footer = new PrintElement(null);
            Footer.AddBlankLine();
            Footer.AddHorizontalRule();
            Footer.AddText("Confidential");
        }

        public void AddPrintObject(IPrintable printObject)
        {
            _printObjects.Add(printObject);
        }
        public String ReplaceTokens(String buf)
        {
            // replace...
            buf = buf.Replace("[pagenum]", _pageNum.ToString());

            // return...
            return buf;
        }
        // OnBeginPrint - called when printing starts
        protected override void OnBeginPrint(PrintEventArgs e)
        {
            // reset...
            _printElements = new ArrayList();
            _pageNum = 0;
            _printIndex = 0;

            // go through the objects in the list and create print elements for each one...
            foreach (IPrintable printObject in _printObjects)
            {
                // create an element...
                PrintElement element = new PrintElement(printObject);
                _printElements.Add(element);

                // tell it to print...
                element.Print();
            }
        }
        protected override void OnPrintPage(PrintPageEventArgs e)
        {
            // adjust the page number...
            _pageNum++;


            // now, render the header element...
            float headerHeight = Header.CalculateHeight(this, e.Graphics);
            Header.Draw(this, e.MarginBounds.Top, e.Graphics, e.MarginBounds);

            // also, we need to calculate the footer height...
            float footerHeight = Footer.CalculateHeight(this, e.Graphics);
            Footer.Draw(this, e.MarginBounds.Bottom - footerHeight, e.Graphics, e.MarginBounds);


            // now we know the header and footer, we can adjust the page bounds...
            Rectangle pageBounds = new Rectangle(e.MarginBounds.Left,
                  (int)(e.MarginBounds.Top + headerHeight), e.MarginBounds.Width,
                  (int)(e.MarginBounds.Height - footerHeight - headerHeight));
            float yPos = pageBounds.Top;


            // ok, now we need to loop through the elements...
            bool morePages = false;
            int elementsOnPage = 0;
            while (_printIndex < _printElements.Count)
            {
                // get the element...
                PrintElement element = (PrintElement)_printElements[_printIndex];


                // how tall is the primitive?
                float height = element.CalculateHeight(this, e.Graphics);

                // will it fit on the page?
                if (yPos + height > pageBounds.Bottom)
                {
                    // we don't want to do this if we're the first thing on the page...
                    if (elementsOnPage != 0)
                    {
                        morePages = true;
                        break;
                    }
                }
                // now draw the element...
                element.Draw(this, yPos, e.Graphics, pageBounds);

                // move the ypos...
                yPos += height;

                // next...
                _printIndex++;
                elementsOnPage++;
            }


            // do we have more pages?
            e.HasMorePages = morePages;
        }

        public void ShowPreview()
        {
            // now, show the print dialog...
            PrintPreviewDialog dialog = new PrintPreviewDialog();
            dialog.Document = this;
            //dialog.UseEXDialog = true; 
            // show the dialog...
            dialog.ShowDialog();
        }
        // ShowPrintDialog - display the print dialog...
        public void ShowPrintDialog()
        {
            // create and show...
            PrintDialog dialog = new PrintDialog();
            dialog.PrinterSettings = PrinterSettings;
            dialog.Document = this;
            if (dialog.ShowDialog() == DialogResult.OK)
            {
                // save the changes...
                PrinterSettings = dialog.PrinterSettings;

                // do the printing...
                Print();
            }
        }
        public void ShowPageSettings()
        {
            PageSetupDialog setup = new PageSetupDialog();
            PageSettings settings = DefaultPageSettings;
            setup.PageSettings = settings;

            // display the dialog and,
            if (setup.ShowDialog() == DialogResult.OK)
                DefaultPageSettings = setup.PageSettings;
        }

    }
    public class Customer : IPrintable
    {
        // members...
        public int Id;
        public String FirstName;
        public String LastName;
        public String Company;
        public String Email;
        public String Phone;

        public void Print(PrintElement element)
        {
            // tell the engine to draw a header...
            element.AddHeader("Customer");

            // now, draw the data...
            element.AddData("Customer ID", Id.ToString());
            element.AddData("Name", FirstName + " " + LastName);
            element.AddData("Company", Company);
            element.AddData("E-mail", Email);
            element.AddData("Phone", Phone);

            // finally, add a blank line...
            element.AddBlankLine();
        }

    }
}