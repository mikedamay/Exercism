using System.Collections.Generic;
using System.Collections.ObjectModel;
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

        public (ReadOnlyDictionary<string, string> learningObjectives, List<string> errors) GetLearningObjectives()
        {
            var ddp = new DesignDocParser();
            var errors = new List<string>();
            var conceptAndObjectivesMap = new Dictionary<string, string>();
            var los = GetExerciseDesigns().SelectMany(d => ddp.ParseDesignDoc(d));
            foreach (var lo in los)
            {
                switch (lo)
                {
                    case (true, _, string concept, string objective):
                        conceptAndObjectivesMap[concept] = objective;
                        break;
                    case (false, string error, _, _):
                        errors.Add(error);
                        break;
                }
            }

            return (new ReadOnlyDictionary<string, string>(conceptAndObjectivesMap), errors);
        }
        
        public IEnumerable<string> GetExerciseDesigns()
        {
            var exercises = Directory.EnumerateDirectories(Path.Combine(root, "languages/csharp/exercises/concept"));
            /*
            var conceptExercises = languages
                .Where(l => Directory.Exists(Path.Combine(l, "exercises", "concept")))
                .SelectMany(l => Directory.EnumerateDirectories(Path.Combine(l, "exercises", "concept")));
            */
            var files = exercises
                .Where(ce => File.Exists(Path.Combine(ce, ".meta", "design.md")));
            var designs = files.Select(ce => File.ReadAllText(Path.Combine(ce, ".meta", "design.md")));
            return designs;
        }
    }
}