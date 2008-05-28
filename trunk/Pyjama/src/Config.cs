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
using System.IO;
using System.Collections.Generic;

class Config {

    public string filename;
    Dictionary<string,Dictionary<string,string>> dictionary = 
	new Dictionary<string,Dictionary<string,string>>();

    public Config(string filename) {
	this.filename = filename;
    }

    public void Load(string filename) {
	this.filename = filename;
	Load();
    }

    public void Load() {
	string line;
	string section = null;
	if (File.Exists(filename)) {
	    StreamReader sr = new StreamReader(filename);
	    while ((line = sr.ReadLine()) != null) {
		line = line.Trim();
		//Console.WriteLine("line = {0}", line);
		if (line.StartsWith("[") && line.EndsWith("]")) {
		    section = line.Substring(1, line.Length - 2);
		    if (!dictionary.ContainsKey(section)) {
			dictionary[section] = new Dictionary<string,string>();
		    }
		} else if (line != "") {
		    string [] parts = line.Split('=');
		    if (parts.Length == 2) {
			string key = parts[0].Trim();
			string value = parts[1].Trim();
			Console.WriteLine("[{0}] {1}={2}", 
					  section, key, value);
			if (section != null) {
			    dictionary[section][key] = value;
			}
		    }
		}
	    }
	    sr.Close();
	} else {
	    Console.WriteLine("No such file: '{0}'", filename);
	}
    }

    void Save(string outfilename) {
	StreamWriter sw = new StreamWriter(outfilename);
	foreach (string section in dictionary.Keys) {
	    sw.WriteLine("[{0}]", section);
	    foreach (string key in dictionary[section].Keys) {
		string value = dictionary[section][key];
		sw.WriteLine("{0}={1}", key, value);
	    }
	    sw.WriteLine();
	}
	sw.Close();
    }

    public static void Main() {
	Config config = new Config("test.ini");
	config.Load();
	config.Save("testout.ini");
    }

}
