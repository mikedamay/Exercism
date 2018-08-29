using System.Collections.Generic;
using System.Linq;

// riffed from gorohoroh's solution
public class GradeSchool
{
    private List<(string Name, int Grade)> students = new List<(string Name, int Grade)>();
    
    public void Add(string student, int grade) => students.Add((student, grade));

    public IEnumerable<string> Roster() => students.OrderBy(x => x.Grade).ThenBy(x => x.Name).Select(x => x.Name);

    public IEnumerable<string> Grade(int grade) => students.Where(x => x.Grade == grade).OrderBy(x => x.Name).Select(x => x.Name);
}