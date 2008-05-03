using Mono.Unix;

class Utils
{
    public static string Tran(string msg) {
      return Catalog.GetString(msg);
    }

    public static string Trans(string msg1, string msg2, int count) {
      return Catalog.GetPluralString(msg1, msg2, count);
    }
}

