using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ExerciseReport
{
    internal interface IDesignDocFileHandler
    {
        IEnumerable<string> GetExerciseDesignsForTrack(string track);
    }
    internal class DesignDocFileHandler : IDesignDocFileHandler
    {
        private readonly string root;

        public DesignDocFileHandler(string root)
        {
            this.root = root;
        }

        public IEnumerable<string> GetExerciseDesignsForTrack(string track)
        {
            var exercisePaths = Directory.EnumerateDirectories(Path.Combine(root, $"languages/{track}/exercises/concept"));
            var designs = exercisePaths
                .Select(exp => Path.Combine(exp, ".meta/design.md"))
                .Where(path => File.Exists(path))
                .Select(path => File.ReadAllText(path));
            return designs;
        }
    }
}