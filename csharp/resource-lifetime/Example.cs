using System;

public class Orm
{
    private Database database;

    public void Write(string data)
    {
        using (var db = new Database())
        {
            db.Write(data);
            db.EndTransaction();
        }
    }

    public bool WriteSafely(string data)
    {
        using var db = new Database();
        try
        {
            db.Write(data);
            db.EndTransaction();

            return true;
        }
        catch (InvalidOperationException)
        {
            return false;
        }
    }
}
