/*********************************************************************
 *
 * Copyright (c) 2008 Douglas S. Blank
 *
 * This source code is subject to terms and conditions of the
 * Microsoft Public License. A copy of the license can be found in the
 * License.html file at the root of this distribution. If you cannot
 * locate the Microsoft Public License, please send an email to
 * dlr@microsoft.com. By using this source code in any fashion, you
 * are agreeing to be bound by the terms of the Microsoft Public
 * License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *********************************************************************/

using System;
using System.Windows.Forms;

public class Pyjama {

  [STAThread] // for windows interop with clipboard, etc
  public static void Main(string[] args) {
	PyjamaForm form = new PyjamaForm(args);
	Application.Run(form);
  }
}