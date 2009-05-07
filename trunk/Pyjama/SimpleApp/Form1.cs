using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace SimpleApp
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void btClose_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        //private void Form1_Load(object sender, EventArgs e)
        //{
            //Here is how you can send commands to IronTextBox via the control itself
            //ironTextBoxControl1.WriteText("help\r\n");
            //ironTextBoxControl1.SimEnter();
        //}
    }
}
