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

    static string applicationDirectory = "pyjama";
    static string configFileName = "config.ini";

    List<string> path = new List<string>();

    Dictionary<string,Dictionary<string,string>> dictionary = 
	new Dictionary<string,Dictionary<string,string>>();

    public static string MakePath(params string [] path) {
	string retval = "";
	char sep = Path.DirectorySeparatorChar;
	foreach (string p in path) {
	    if (retval != "")
		retval += sep;
	    retval += p;
	}
	return retval;
    }

    public Config() {
	// Environment.SpecialFolder.CommonApplicationData: This is
	// the directory for storing information that is shared by all
	// users on all machines.

	// Environment.SpecialFolder.ApplicationData: This is the
	// directory for the current user, shared by all machines on
	// the network.

	// Environment.SpecialFolder.LocalApplicationData: This is the
	// directory for the current user that is only available when
	// logged on to this machine.

	// From general to specific:
	path.Add(MakePath(
           Environment.GetFolderPath(
               Environment.SpecialFolder.CommonApplicationData),
	   applicationDirectory, configFileName));
	path.Add(MakePath(
           Environment.GetFolderPath(
	       Environment.SpecialFolder.ApplicationData),
	   applicationDirectory, configFileName));
	path.Add(MakePath(
           Environment.GetFolderPath(
               Environment.SpecialFolder.LocalApplicationData),
	   applicationDirectory, configFileName));
	if (Environment.GetEnvironmentVariable("PYJAMA") != null) {
	    path.Add(MakePath(Environment.GetEnvironmentVariable("PYJAMA"),
			      applicationDirectory, configFileName));
	}
	path.Add(MakePath(Environment.CurrentDirectory,
			  applicationDirectory, configFileName));
	path.Add(MakePath(Environment.CurrentDirectory,
			  configFileName));
	foreach (string filename in path) {
	    Console.WriteLine("Checking '{0}...'", filename);
	    if (File.Exists(filename)) {
		Load(filename);
	    }
	}
    }

    public void AddSection(string section) {
	if (!dictionary.ContainsKey(section)) {
	    dictionary[section] = new Dictionary<string,string>();
	}
    }

    public void Add(string section, string key, string value) {
	AddSection(section);
	dictionary[section][key] = value;
    }

    public void Load(string filename) {
	string line;
	string section = null;
	if (File.Exists(filename)) {
	    Console.WriteLine("   Loading...");
	    StreamReader sr = new StreamReader(filename);
	    while ((line = sr.ReadLine()) != null) {
		line = line.Trim();
		if (line.StartsWith(";") || line.StartsWith("#")) {
		    // pass, comment
		} else if (line.StartsWith("[") && line.EndsWith("]")) {
		    section = line.Substring(1, line.Length - 2);
		    if (!dictionary.ContainsKey(section)) {
			dictionary[section] = new Dictionary<string,string>();
		    }
		} else if (line != "") {
		    string [] parts = line.Split('=');
		    if (parts.Length == 2) {
			string key = parts[0].Trim();
			string value = parts[1].Trim();
			//Console.WriteLine("[{0}] {1}={2}", 
			//section, key, value);
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

    void Save() {
	Save(configFileName, null);
    }

    void Save(string outfilename, string directory) {
	if (directory == null)
	    directory = MakePath(Environment.GetFolderPath(
			     Environment.SpecialFolder.LocalApplicationData),
				 applicationDirectory);
	if (!Directory.Exists(directory)) {
	    try {
		Directory.CreateDirectory(directory);	
	    } catch {
		Console.WriteLine("Error! Can't make folder '{0}'", directory);
		return;
	    }
	}
	StreamWriter sw = new StreamWriter(MakePath(directory, outfilename));
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
	Config config = new Config();
	config.Add("recent", "file0", @"c:\\test0.py");
	config.Add("recent", "file1", @"c:\\test1.py");
	config.Add("recent", "file2", @"c:\\test2.py");
	config.Save();
    }

}
