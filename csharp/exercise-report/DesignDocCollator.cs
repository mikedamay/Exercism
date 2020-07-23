using System.Collections.Generic;
using System.IO;
using System.Linq;
using FSharp.Compiler.SourceCodeServices;

namespace ExerciseReport
{
    internal class DesignDocCollator
    {
        private readonly string root;
        private readonly DesignDocParser designDocParser;

        public DesignDocCollator(string root, DesignDocParser designDocParser)
        {
            this.root = root;
            this.designDocParser = designDocParser;
        }

        public (LearningObjectives learningObjectives, List<string> errors) GetLearningObjectives(string track)
        {
            var errors = new List<string>();
            var learningObjectives = new LearningObjectives();
            var conceptsAndObjectives = GetExerciseDesignsForTrack(track).SelectMany(d => designDocParser.ParseDesignDoc(d));
            foreach (var conceptAndObjective in conceptsAndObjectives)
            {
                switch (conceptAndObjective)
                {
                    case (true, _, string concept, string objective):
                        learningObjectives.Builder.Add(concept, objective);
                        break;
                    case (false, string error, _, _):
                        errors.Add(error);
                        break;
                }
            }

            return (learningObjectives, errors);
        }
        
        private IEnumerable<string> GetExerciseDesignsForTrack(string track)
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