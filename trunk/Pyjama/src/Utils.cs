using Mono.Unix;

class Utils
{
    public static string Tran(string msg) {
      return Catalog.GetString(msg);
    }

    public static string Trans(string msg1, string msg2, int count) {
      return Catalog.GetPluralString(msg1, msg2, count);
    }

    public static string GetMimeType(string extension) 
    {
	string mime_type = "text/plain";
	switch(extension) {
	case ".cs":
	    mime_type = "text/x-csharp";
	    break;
	case ".cpp":
	case ".cc":
	case ".h":
	    mime_type = "text/x-cpp";
	    break;
	case ".c":
	    mime_type = "text/x-c";
	    break;
	case ".xml":
	    mime_type = "text/xml";
	    break;
	case ".css":
	    mime_type = "text/css";
	    break;
	case ".js":
	    mime_type = "text/x-javascript";
	    break;
	case ".boo":
	    mime_type = "text/x-boo";
	    break;
	case ".vb":
	    mime_type = "text/x-vb";
	    break;
	case ".java":
	    mime_type = "text/x-java";
	    break;
	case ".sh":
	    mime_type = "text/x-sh";
	    break;
	case ".sql":
	    mime_type = "text/x-sql";
	    break;
	case ".d":
	    mime_type = "text/x-dsrc";
	    break;
	case ".perl":
	case ".pl":
	    mime_type = "text/x-perl";
	    break;
	case ".php":
	    mime_type = "text/x-php";
	    break;
	case ".py":
	case ".python":
	    mime_type = "text/x-python";
	    break;
	case ".html":
	case ".htm":
	case ".shtml":
	    mime_type = "text/html";
	    break;
	case ".txt":
	default:
	    mime_type = "text/plain";
	    break;
	}
	return mime_type;
    }
}

