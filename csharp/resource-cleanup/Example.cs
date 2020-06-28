namespace example
{
using System;

// **** please do not modify the Database class ****
// this demonstrates the "dispose pattern" appropriate when
// a class acquires external resources.
public class Database : IDisposable
{
    public enum State {TransactionStarted, DataWritten, Invalid, Closed}

    // extern object AcquireAnUnmanagedResource();    // allocates space on disk etc.
    // extern bool ReleaseTheUnmanagedResource(object resource);
    
    public State DbState { get; private set; } = State.Closed;
    private object resource;
    public string lastData;
    public void BeginTransaction()
    {
        //resource = AcquireAnUnmanagedResource();
        DbState = State.TransactionStarted;
    }

    public void Write(string data)
    {
        // this does something significant with the unmanaged resource
        lastData = data;
        if (data == "bad write")
        {
            DbState = State.Invalid;
            throw new InvalidOperationException();
        }

        DbState = State.DataWritten;
    }
    
    public void EndTransaction()
    {
        if (lastData == "bad commit")
        {
            DbState = State.Invalid;
            throw new InvalidOperationException();
        }

        DbState = State.Closed;
    }

    private void Release()
    {
        // ReleaseTheUnmanagedResource(resource);
    }

    public void Dispose()
    {
        DbState = State.Closed;
        Release();
        GC.SuppressFinalize(this);
    }

    ~Database()
    {
        Dispose();
    }
}

public class Orm : IDisposable
{
    private Database database;

    public Orm(Database database)
    {
        this.database = database;
    }

    public void Begin()
    {
        database.BeginTransaction();
    }

    public void Write(string data)
    {
        try
        {
            database.Write(data);
        }
        catch (Exception)
        {
            database.Dispose();
        }
    }

    public void Commit()
    {
        try
        {
            database.EndTransaction();
        }
        catch (Exception)
        {
            database.Dispose();
        }
    }

    public void Dispose()
    {
        if (database != null)
        {
            database.Dispose();
        }
    }
}
}