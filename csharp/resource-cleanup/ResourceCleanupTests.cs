using System;
using System.Linq;
using System.Reflection;
using Xunit;
using example;

public class ResourceCleanupTests
{
    [Fact]
    public void Write_good()
    {
        var db = new Database();
        var orm = new Orm(db);
        orm.Write("good write");
        object[] results = {db.DbState, db.lastData};
        Assert.Equal(new object[] { Database.State.DataWritten, "good write"}, results);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Write_bad()
    {
        var db = new Database();
        var orm = new Orm(db);
        orm.Write("bad write");
        object[] results = {db.DbState, db.lastData};
        Assert.Equal(new object[] { Database.State.Closed, "bad write"}, results);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Commit_good()
    {
        var db = new Database();
        var orm = new Orm(db);
        orm.Write("good commit");
        orm.Commit();
        object[] results = {db.DbState, db.lastData};
        Assert.Equal(new object[] { Database.State.Closed, "good commit"}, results);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Commit_bad()
    {
        var db = new Database();
        var orm = new Orm(db);
        orm.Write("bad commit");
        orm.Commit();
        object[] results = {db.DbState, db.lastData};
        Assert.Equal(new object[] { Database.State.Closed, "bad commit"}, results);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Disposable()
    {
        var db = new Database();
        var orm = new Orm(db);
        orm.Write("good data");
        bool disposable = orm is IDisposable;
        string lastData = string.Empty;
        if (disposable)
        {
            typeof(Orm).InvokeMember("Dispose",
                BindingFlags.Public | BindingFlags.Instance | BindingFlags.InvokeMethod,
                null, orm, null);
        }

        object[] results = {disposable, db.DbState, db.lastData};
        Assert.Equal(new object[] {true, Database.State.Closed, "good data"}, results);
    }
}
