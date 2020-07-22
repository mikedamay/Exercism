using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ExerciseReport
{
    internal class DesignDocCollator
    {
        private readonly string root;

        public DesignDocCollator(string root)
        {
            this.root = root;
        }

        public IEnumerable<string> GetDesignFileNames()
        {
            var languages = Directory.EnumerateDirectories(Path.Combine(root, "languages"));
            var conceptExercises = languages
                .Where(l => Directory.Exists(Path.Combine(l, "exercises", "concept")))
                .SelectMany(l => Directory.EnumerateDirectories(Path.Combine(l, "exercises", "concept")));
            var files = conceptExercises.Where(ce => File.Exists(Path.Combine(ce, ".meta", "design.md")));
            var designs = files.Select(ce => File.ReadAllText(Path.Combine(ce, ".meta", "design.md")));
            return designs;
        }
        
    }
}